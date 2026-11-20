{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- The static analysis: every module checked as written, all of it, before anything is
-- evaluated, so a mistake in a function no graph calls, or in a branch not taken, is reported
-- too (see the org note, "Static analysis"). It checks names, that every name resolves where
-- it is written, and every refinement's name names a refinement; calls, that a call of a known
-- function has its arity; and recursion, that a function reaching itself by its name, or a
-- refinement by an assertion, goes down into a value each time round, so it finishes.
-- The scope rules are the evaluator's, and what a form is and what it binds come from
-- 'Analysis.Forms', which the evaluator uses too.
module Data.Ouro.Lisp.Analysis
( analyseModule
) where

import           Data.Containers.ListUtils     (nubOrd)
import           Data.Foldable                 (asum)
import           Data.Function                 ((&))
import           Data.Graph                    (SCC (..), stronglyConnComp)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Ouro.Error.Diagnostics   (incorrectArity, notARefinement,
                                                notExported, recursiveCall,
                                                unboundName, unknownRefinement,
                                                unknownQualifier, withBlurb)
import           Data.Ouro.Error.Types         (OuroError (..))
import           Data.Ouro.Lisp.Analysis.Forms (BlockTarget (..), FrameEntry (..),
                                                collectBindings,
                                                determineBlockTarget,
                                                hasValidOtherwise, holeName,
                                                isRestHole, isSubjectHole,
                                                letBindings, param, shadowsForm)
import           Data.Ouro.Lisp.Eval.Builtins  (builtinArity, builtinRegistry)
import qualified Data.Ouro.Lisp.Eval.Types     as L
import           Data.Ouro.Lisp.Function       (checkParams, functionBody,
                                                isFunctionKeyword)
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import           Data.Ouro.Lisp.Module.Types   (Declaration (..),
                                                FunctionKind (..),
                                                HigherExpression (..), Module,
                                                SomeFunction (..),
                                                functionRegistry, graphRegistry,
                                                splitQualified)
import qualified Data.Ouro.Lisp.Surface        as S
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Text.Megaparsec               (SourcePos)


-- A function that can be reached by its name: a function of the module, or a fn bound by name,
-- by a let binding or an attribute pair, where it is bound
data Node
    = Defn  Text
    | Bound Text SourcePos
    deriving (Eq, Ord)

nodeName :: Node -> Text
nodeName = \case
    Defn  name   -> name
    Bound name _ -> name

-- How a value relates to one of a function's parameters: no larger (the value itself, or
-- something no larger), or smaller, a part of it
data Rel = NoLarger | Smaller
    deriving (Eq, Ord)

-- What is known of a value's size: how it relates to each parameter of the function it is
-- written in, by the parameter's place. Nothing known is no entry.
type Size = Map.Map Int Rel

-- A part of a value: smaller than whatever the value is no larger than
partOf :: Map.Map k Rel -> Map.Map k Rel
partOf = Map.map (const Smaller)

-- A value no larger than another
noLargerThan :: Size -> Size
noLargerThan = Map.map (const NoLarger)

-- A function's name written inside another function (or itself), where it is written: a call,
-- with the size of each argument, or the function named otherwise, its arguments unknown
data Call = Call
    { callFrom :: Node
    , callTo   :: Node
    , callPos  :: SourcePos
    , callArgs :: Maybe [Size]
    }

-- What the walk finds: errors, and the calls by name between functions
data Found = Found [OuroError] [Call]

instance Semigroup Found where
    Found errs calls <> Found errs' calls' = Found (errs <> errs') (calls <> calls')

instance Monoid Found where
    mempty = Found [] []

problem :: OuroError -> Found
problem err = Found [err] []


-- What a name can refer to where it is written: the names bound around it (attribute pairs,
-- let bindings, parameters, holes), each with the function it names, if it binds a fn; the
-- module's own functions; and the modules it imports, by qualifier. The built-ins come after
-- them all. The function being written, if any, is where a name of another function calls it,
-- and the names whose size is known are known against its parameters.
data Scope = Scope
    { scopeLocals    :: Map.Map Text (Maybe Node)
    , scopeFunctions :: L.FunctionRegistry
    , scopeImports   :: Map.Map Text L.Imported
    , scopeOwner     :: Maybe Node
    , scopeSizes     :: Map.Map Text Size
    }

-- Whether a binding around this point has the name: a local or a function of the module
bound :: Scope -> Text -> Bool
bound scope name = Map.member name (scopeLocals scope) || Map.member name (scopeFunctions scope)

-- The scope with more names bound around it, none of them a function's
within :: Set.Set Text -> Scope -> Scope
within more = withinFrame (Map.fromSet (const Nothing) more)

-- The scope with a frame's names bound around it, of no known size
withinFrame :: Map.Map Text (Maybe Node) -> Scope -> Scope
withinFrame more scope = scope { scopeLocals = more <> scopeLocals scope
                               , scopeSizes  = scopeSizes scope `Map.difference` more }

-- The scope with the sizes of some of its names known
sized :: Map.Map Text Size -> Scope -> Scope
sized sizes scope = scope { scopeSizes = Map.filter (not . Map.null) sizes <> scopeSizes scope }

-- The scope inside a function's body, where sizes are known against its parameters
inside :: Node -> Scope -> Scope
inside node scope = scope { scopeOwner = Just node, scopeSizes = Map.empty }

-- The function a name names, where it is written: one bound around it by name, or one of the
-- module; not a built-in, nor an import's, which cannot call back
named :: Scope -> Text -> Maybe Node
named scope name =
    case Map.lookup name (scopeLocals scope) of
        Just local -> local
        Nothing    -> case Map.member name (scopeFunctions scope) of
                          True  -> Just (Defn name)
                          False -> Nothing


-- Every error in a module's functions and graphs, in source order, with the modules it
-- imports, by qualifier; and the calls by name found to go down into a value, by where their
-- function's name is written, which the evaluator lets call a function already being called
analyseModule :: Map.Map Text L.Imported -> Module -> ([OuroError], Set.Set SourcePos)
analyseModule imported ouroModule =
    (sortOn errPos (nubOrd (errs <> circleErrors)), accepted)
    where
    (circleErrors, accepted) = recursion calls

    Found errs calls = foldMap function (Map.elems (ouroModule ^. functionRegistry))
                    <> foldMap graph    (Map.elems (ouroModule ^. graphRegistry))

    root = Scope Map.empty (ouroModule ^. functionRegistry) imported Nothing Map.empty

    -- A body the parser rejected is reported by the parser. The return type is read as an
    -- assertion on the body, which is how it is checked: a call of its refinement on the result
    function :: SomeFunction -> Found
    function (SomeFunction (Function _ pos name _ params resultType body)) =
        case body of
            S.Form _ exprs | Right result <- functionBody name pos exprs -> parameters (inside (Defn name) root) ownParameters params (returned resultType result)
            _rejected                                                     -> mempty

    returned resultType result = maybe result (\(tPos, ty) -> S.Asserted tPos ty result) resultType

    graph :: HigherExpression 'GraphExpr -> Found
    graph (Graph _ _ body) = names root body

-- Each parameter of a function, as its own body sees it: itself
ownParameters :: [Size]
ownParameters = [ Map.singleton i NoLarger | i <- [0 ..] ]


-- A function that reaches itself by its name, directly or through others, must go down into a
-- value each time round: Lee, Jones and Ben-Amram's size-change termination (2001). Each call
-- in a circle of functions relates the arguments it passes to the caller's parameters; the
-- circle finishes when every way round it, followed until it repeats itself, passes some
-- parameter a part of itself. One that might not finish is reported once, at the name that
-- closes it: the last written. The calls of the circles that finish are accepted, by where
-- they are written. A function reached through a value, a parameter, or a function a call
-- returns, is the evaluator's to check, when it is called.
recursion :: [Call] -> ([OuroError], Set.Set SourcePos)
recursion calls =
    ( [ OuroError (callPos closing) (recursiveCall (map nodeName (route next (callTo closing) (callFrom closing) <> [callTo closing])))
      | (False, within') <- circles
      , closing : _ <- [reverse (sortOn callPos within')]
      ]
    , Set.fromList [ callPos call | (True, within') <- circles, call <- within' ]
    )
    where
    circles = [ (finishes within', within')
              | CyclicSCC circle <- stronglyConnComp [ (node, node, Map.findWithDefault [] node next) | node <- Map.keys next ]
              , let members = Set.fromList circle
                    within' = [ call | call <- calls, callFrom call `Set.member` members, callTo call `Set.member` members ]
              ]

    next = Map.fromListWith (<>) ([ (callFrom call, [callTo call]) | call <- calls ] <> [ (callTo call, []) | call <- calls ])


-- How one call, or a run of calls, relates the parameters of the function it starts in to
-- those of the function it ends in: an arc from a parameter to one, when the argument passed
-- for it is known against the first
data Change = Change Node Node (Map.Map (Int, Int) Rel)
    deriving (Eq, Ord)

-- Whether every way round a circle of calls finishes: of the changes its runs of calls make,
-- each that starts and ends in one function and stays the same when run twice passes some
-- parameter a part of itself
finishes :: [Call] -> Bool
finishes calls = and [ descends arcs | change@(Change from to arcs) <- Set.toList (closure (Set.fromList (map changeOf calls)))
                                     , from == to
                                     , andThen change change == Just change ]
    where
    changeOf call = Change (callFrom call) (callTo call) $
                    Map.fromListWith max [ ((i, j), rel) | (j, argument) <- zip [0 ..] (fromMaybe [] (callArgs call))
                                                         , (i, rel) <- Map.toList argument ]

    descends arcs = or [ rel == Smaller | ((i, j), rel) <- Map.toList arcs, i == j ]

    -- Every run of calls, until no new change is made: finitely many, since a change is two
    -- functions and arcs between their parameters
    closure changes = case changes <> Set.fromList (mapMaybe (uncurry andThen) [ (a, b) | a <- Set.toList changes, b <- Set.toList changes ]) of
                          more | more == changes -> changes
                               | otherwise       -> closure more

-- One run of calls, then another that starts where it ends
andThen :: Change -> Change -> Maybe Change
andThen (Change from middle arcs) (Change middle' to arcs') =
    case middle == middle' of
        True  -> Just (Change from to (Map.fromListWith max [ ((i, k), max rel rel') | ((i, j), rel) <- Map.toList arcs
                                                                                     , ((j', k), rel') <- Map.toList arcs'
                                                                                     , j == j' ]))
        False -> Nothing


-- The functions from one to another, both included, following calls
route :: Map.Map Node [Node] -> Node -> Node -> [Node]
route next start goal = fromMaybe [start, goal] (search Set.empty start)
    where
    search seen node
        | node == goal             = Just [goal]
        | node `Set.member` seen   = Nothing
        | otherwise                = (node :) <$> asum (map (search (Set.insert node seen)) (Map.findWithDefault [] node next))


-- A function's parameters, and the holes of their patterns, are bound around its body, with
-- each parameter's size, as the function is called. The values in a pattern are read in the
-- scope the function is written in, which cannot see its parameters. A malformed pattern
-- stops the call before the body, so the body is not looked into. A parameter's refinement
-- is asserted on an argument of a size unknown here.
parameters :: Scope -> [Size] -> [S.Param] -> S.Expr -> Found
parameters closure sizes params body =
    case mconcat <$> traverse (pattern closure) [ p | Just p <- map S.paramPattern params ] of
        Nothing                    -> mempty
        Just (patternFound, holes) -> contracts <> patternFound <> names (sized given (within (Set.fromList (map S.paramName params) <> holes) closure)) body
    where
    contracts = mconcat [ typeNames closure tPos ty Nothing | S.Param _ (Just (tPos, ty)) _ <- params ]

    given = Map.unions [ Map.insert (S.paramName p) argument (foldMap (`holeSizes` argument) (S.paramPattern p))
                       | (p, argument) <- zip params (sizes <> repeat Map.empty) ]


-- A case or parameter pattern, as 'matchPattern' reads it: what is found in the values it
-- compares by, and the names its holes bind; or Nothing when it is malformed, which is the
-- evaluator's error: a hole inside a value, a Record pattern with anything but ':key pattern'
-- entries and one '?..rest', or a '?..rest' before the end of an Array pattern.
pattern :: Scope -> S.Expr -> Maybe (Found, Set.Set Text)
pattern scope = \case
    S.Hole _ hole
        -> Just (mempty, holeBinds hole)

    S.Form _ elems
        | any S.isAttrForm elems
        -> let keyed  = [ p | S.Form _ [S.Symbol _ "%attr", S.Literal _ (S.Str _), p] <- elems ]
               rests  = [ hole | S.Hole _ hole <- elems, isRestHole hole ]
               strays = length elems - length keyed - length rests
           in case (strays, rests) of
                  (0, [])     -> mconcat <$> traverse (pattern scope) keyed
                  (0, [rest]) -> (<> (mempty, holeBinds rest)) . mconcat <$> traverse (pattern scope) keyed
                  _malformed  -> Nothing

    p@(S.Form _ (S.Symbol _ _ : _))
        -> byValue p

    S.Form _ elems
        | any isRestHole [ hole | S.Hole _ hole <- drop 1 (reverse elems) ]
        -> Nothing
        | otherwise
        -> mconcat <$> traverse (pattern scope) elems

    p   -> byValue p

    where
    holeBinds hole = foldMap Set.singleton (holeName hole)

    byValue p = case S.containsHole p of
                    True  -> Nothing
                    False -> Just (names scope p, Set.empty)


-- The sizes of the holes of a pattern matched against a value of a known size: a hole in it
-- is a part of the value, but for a hole that is the whole pattern, or a rest hole alone in
-- its pattern, which may be all of it
holeSizes :: S.Expr -> Size -> Map.Map Text Size
holeSizes p argument = Map.map (\case Smaller -> partOf argument; NoLarger -> argument) (parts p)
    where
    parts = \case
        S.Hole _ hole        -> named' NoLarger hole
        S.Asserted _ _ inner -> parts inner
        S.Form _ elems       -> Map.unions (map (element (length elems)) elems)
        _noHole              -> Map.empty

    element count = \case
        S.Hole _ hole
            | isRestHole hole, count == 1 -> named' NoLarger hole
            | otherwise                   -> named' Smaller hole
        S.Asserted _ _ inner              -> element count inner
        S.Form _ [S.Symbol _ "%attr", _, inner]
                                          -> partOf (parts inner)
        nested                            -> partOf (parts nested)

    named' rel hole = foldMap (`Map.singleton` rel) (holeName hole)


-- What is known of the size of an expression's value, against the parameters of the function
-- it is written in: a name of known size; a path into a value, a part of it; and get-or with
-- an empty fallback, image and filter, no larger than the value they look into
size :: Scope -> S.Expr -> Size
size scope = \case
    S.Symbol _ name      -> Map.findWithDefault Map.empty name (scopeSizes scope)
    S.Asserted _ _ inner -> size scope inner
    S.Form _ items
        | shadowsForm (bound scope) items                         -> Map.empty
    S.Form _ (S.Symbol _ "get" : target : _ : _)                  -> partOf (size scope target)
    S.Form _ (S.Symbol _ "get-or" : target : fallback : _ : _)
        | empty fallback                                          -> noLargerThan (size scope target)
    S.Form _ (S.Symbol _ "image" : target : _ : _)                -> noLargerThan (size scope target)
    S.Form _ [S.Symbol _ "filter", xs, _]                         -> noLargerThan (size scope xs)
    _unknown                                                      -> Map.empty
    where
    empty = \case
        S.Literal _ S.EmptyArr -> True
        S.Literal _ S.EmptyRec -> True
        S.Form _ []            -> True
        _notEmpty              -> False

-- An element of an Array of known size is a part of it
elementSize :: Scope -> S.Expr -> Size
elementSize scope = partOf . size scope


-- Every name in an expression resolves where it is written
names :: Scope -> S.Expr -> Found
names scope = \case
    S.Symbol pos name       -> resolve scope pos name
    S.Asserted pos ty inner -> typeNames scope pos ty (Just [size scope inner]) <> names scope inner
    S.Form pos items        -> form scope pos items
    _leaf                   -> mempty


-- The refinements a type names, each resolved as 'refinementName' resolves one, however deep in
-- ∪ ∩ ∖ it is written. An array's element type is about its elements, and a record type's key
-- types about the values at its keys, each a part of the subject, so a refinement asserted on
-- them goes down into the value
typeNames :: Scope -> SourcePos -> S.Type -> Maybe [Size] -> Found
typeNames scope pos ty args = case ty of
    S.Named refinement -> refinementName scope pos refinement args
    S.Union a b        -> typeNames scope pos a args <> typeNames scope pos b args
    S.Inter a b        -> typeNames scope pos a args <> typeNames scope pos b args
    S.Minus a b        -> typeNames scope pos a args <> typeNames scope pos b args
    S.ArrayOf element  -> typeNames scope pos element (fmap (map partOf) args)
    S.RecordOf pairs   -> foldMap (\(_, t) -> typeNames scope pos t (fmap (map partOf) args)) pairs


-- A refinement's name names a refinement of the module, titled: never a binding around it,
-- since nothing bound is a refinement. Unknown, it is SYN0000, with the nearest refinement or
-- built-in name; a function of the procedure kind is not one. An assertion is a call of its
-- refinement on the value it asserts, of the size given (unknown for a signature's), so a
-- refinement asserting itself must go down into a value too. An imported refinement,
-- people/titled, is not yet read, and a built-in refinement names nothing.
refinementName :: Scope -> SourcePos -> S.Refinement -> Maybe [Size] -> Found
refinementName scope pos refinement args = case refinement of
    S.RDefined name
        -> case Map.lookup name (scopeFunctions scope) of
               Just (SomeFunction (Function RefinementKind _ _ _ _ _ _)) -> naming scope (Defn name) pos args
               Just (SomeFunction (Function ProcedureKind _ _ _ _ _ _))  -> problem (OuroError pos (notARefinement name))
               Nothing                                                   -> problem (OuroError pos (unknownRefinement name refinements))
    _builtIn
        -> mempty
    where
    refinements = [ name | SomeFunction (Function RefinementKind _ name _ _ _ _) <- Map.elems (scopeFunctions scope) ]


-- A name: one bound around it, a function of the module, or a built-in; or, qualified, a
-- function an import exports. Unbound, it is SCP0000, with the nearest name in scope, the
-- names Canon gives its own bindings ('%target', '%1') aside. A function's name, written in
-- a function but not called there, names it with arguments unknown.
resolve :: Scope -> SourcePos -> Text -> Found
resolve scope pos name
    | Just (qualifier, unqualified) <- splitQualified name
        = case Map.lookup qualifier (scopeImports scope) of
              Nothing
                  -> problem (OuroError pos (unknownQualifier qualifier (Map.keys (scopeImports scope)) (Map.keys bundled)))
              Just imported
                  | unqualified `Set.member` exported -> mempty
                  | otherwise                         -> problem (OuroError pos (notExported qualifier unqualified (Set.toList exported)))
                  where
                  exported = L.importedExports imported
    | bound scope name || Map.member name builtinRegistry
        = foldMap (\node -> naming scope node pos Nothing) (named scope name)
    | otherwise
        = problem (OuroError pos (unboundName name [ n | n <- inScope, not ("%" `T.isPrefixOf` n) ]))
    where
    inScope = Map.keys (scopeLocals scope) <> Map.keys (scopeFunctions scope)

-- A function named where it is written, from the function being written, if any
naming :: Scope -> Node -> SourcePos -> Maybe [Size] -> Found
naming scope node pos args = case scopeOwner scope of
                                Just owner -> Found [] [Call owner node pos args]
                                Nothing    -> mempty


-- A call: the function its head names, with the size of each argument, or its head read as
-- any other name
callForm :: Scope -> [S.Expr] -> Found
callForm scope = \case
    S.Symbol pos name : args
        | Just node <- named scope name -> naming scope node pos (Just (map (size scope) args)) <> foldMap (names scope) args
    items                               -> foldMap (names scope) items

-- A function map, exists, forall, omits, filter or '->' calls with one argument of a known
-- size, and fold with two: one it names, or a fn written there, whose parameters are those
-- arguments
applied :: Scope -> [Size] -> S.Expr -> Found
applied scope arguments = \case
    S.Symbol pos name
        | Just node <- named scope name -> naming scope node pos (Just arguments)
    S.Form _ items
        | Just found <- lambda scope arguments items
                                        -> found
    f                                   -> names scope f

-- A fn, with the sizes of its parameters as it is called; Nothing when the form is not one.
-- Its parameters are checked when it is written, and one the evaluator rejects is its error.
lambda :: Scope -> [Size] -> [S.Expr] -> Maybe Found
lambda scope sizes items = case items of
    _shadowed
        | shadowsForm (bound scope) items                         -> Nothing
    [S.Symbol fnPos (S.formName -> "fn"), S.Form _ params, body]
        | Just typed <- traverse param (zip [1 ..] params)        -> Just (checked fnPos typed body)
    [S.Symbol fnPos (S.formName -> "fn"), S.Asserted tPos ty (S.Form _ params), body]
        | Just typed <- traverse param (zip [1 ..] params)        -> Just (typeNames scope tPos ty Nothing <> checked fnPos typed body)
    S.Symbol _ (S.formName -> "fn") : _                           -> Just mempty
    _notAFn                                                       -> Nothing
    where
    checked fnPos typed body = case checkParams fnPos typed of
                                   Right () -> parameters scope sizes typed body
                                   Left _   -> mempty


-- A form, read as the evaluator reads it (see 'Engine.evalExpr'). A form the evaluator rejects
-- whole, without evaluating what is in it, is not looked into: its own error is the
-- evaluator's, where it is evaluated.
form :: Scope -> SourcePos -> [S.Expr] -> Found
form scope pos items = case items of
    -- A special form's name that a binding around the form has is the binding's, as in Scheme:
    -- the form is an ordinary call of it
    _shadowed
        | shadowsForm (bound scope) items             -> arity scope pos items <> callForm scope items

    S.Symbol _ "context" : _                         -> mempty
    S.Symbol _ "attr" : _                            -> mempty

    -- A path's keys are not names
    S.Symbol _ "get" : target : _path                -> names scope target
    S.Symbol _ "has" : target : _ : _                -> names scope target
    S.Symbol _ "has" : _                             -> mempty
    S.Symbol _ "get-or" : target : fallback : _ : _  -> names scope target <> names scope fallback
    S.Symbol _ "get-or" : _                          -> mempty
    S.Symbol _ "image" : target : _ : _              -> names scope target
    S.Symbol _ "image" : _                           -> mempty

    S.Symbol _ "array" : elements                    -> foldMap (names scope) elements
    S.Symbol _ "make" : S.Symbol _ "record" : rItems -> record scope rItems
    S.Symbol _ "make" : S.Symbol _ "array" : aItems  -> array scope aItems
    S.Symbol _ "make" : _                            -> mempty
    [S.Symbol _ "map", xs, f]                        -> names scope xs <> applied scope [elementSize scope xs] f
    S.Symbol _ "map" : _                             -> mempty
    [S.Symbol _ quantifier, xs, f]
        | quantifier `elem` quantifiers              -> names scope xs <> applied scope [elementSize scope xs] f
    S.Symbol _ quantifier : _
        | quantifier `elem` quantifiers              -> mempty
    -- The running value is of no size known here; each element is a part of the Array
    [S.Symbol _ "fold", xs, start, f]                -> names scope xs <> names scope start <> applied scope [Map.empty, elementSize scope xs] f
    S.Symbol _ "fold" : _                            -> mempty
    [S.Symbol _ connective, xs]
        | connective `elem` ["all", "any"]           -> names scope xs
    S.Symbol _ connective : _
        | connective `elem` ["all", "any", "and", "or"] -> mempty
    [S.Symbol _ "%apply", f, x]                      -> applied scope [size scope x] f <> names scope x
    [S.Symbol _ "->"]                                -> mempty
    [S.Symbol _ "with"]                              -> mempty
    [S.Symbol _ "adjoin"]                            -> mempty

    S.Symbol _ (S.formName -> "let") : [S.Form _ bindings, body]
        | Right entries <- letBindings bindings      -> frame scope entries <> names (sized (frameSizes scope entries) (withinFrame (frameNames entries) scope)) body
    S.Symbol _ (S.formName -> "let") : _             -> mempty

    S.Symbol _ "when" : _                            -> mempty
    [S.Symbol _ (S.asks -> True), subject, S.Asserted tPos ty (S.Hole _ "%subject")]
        | not (isSubjectHole subject)                -> typeNames scope tPos ty (Just [size scope subject]) <> names scope subject
    S.Symbol _ (S.formName -> "satisfies") : _       -> mempty
    S.Symbol _ "assert" : _                          -> mempty
    S.Symbol _ "in" : _                              -> mempty
    S.Symbol _ keyword : _
        | isFunctionKeyword keyword                  -> mempty
    [S.Symbol _ "%binder", S.Literal _ (S.Str _)]    -> mempty

    S.Symbol _ (S.formName -> "fn") : _              -> fromMaybe mempty (lambda scope [] items)

    [S.Symbol _ "%attr", S.Literal _ (S.Str _), value]
                                                     -> names scope value

    S.Symbol _ (S.formName -> "case") : target : branches
        | hasValidOtherwise branches                 -> names scope target <> foldMap (branch scope (size scope target)) branches
        | otherwise                                  -> mempty

    S.Symbol _ "nth" : _                             -> mempty
    [S.Symbol _ op, coll, i, x]
        | op `elem` ["insert", "replace-at"]         -> foldMap (names scope) [coll, i, x]

    -- Anything else is what its elements make it. A call's head is a name like any other.
    _otherForm -> case determineBlockTarget items of
                      TargetRecord      -> record scope items
                      TargetList        -> array scope items
                      TargetFunctionApp -> arity scope pos items <> callForm scope items

    where
    quantifiers = ["exists", "forall", "omits", "filter"]


-- A call of a known function has as many arguments as the function has parameters (SCP0002),
-- reported at the call with how the function is called, as the evaluator reports it, and a
-- call of a built-in as many as it takes (see 'builtinArity'). Only a call written with the
-- function's name is known here: a function reached through a value, a parameter, a let
-- binding or map, is checked when it is called.
arity :: Scope -> SourcePos -> [S.Expr] -> Found
arity scope pos = \case
    S.Symbol _ name : args
        | Just (called, params) <- known scope name
        , length params /= length args
        -> incorrectArity called (length params) (length args)
           & maybe id (\template -> withBlurb ("It is called " <> template <> ".")) (L.callTemplate called params)
           & OuroError pos
           & problem
    -- A built-in, unless a binding around the call or a function of the module has its name,
    -- takes the number of arguments it is listed with, as the evaluator would report
    S.Symbol _ name : args
        | Nothing <- known scope name
        , not (bound scope name)
        , Just (expected, hint) <- Map.lookup name builtinArity
        , expected /= length args
        -> incorrectArity name expected (length args)
           & withBlurb hint
           & OuroError pos
           & problem
    _otherCall
        -> mempty


-- The function a name at the head of a call names, by its own name, with its parameters: one
-- of the module, unless a binding around the call has the name, or one an import exports
known :: Scope -> Text -> Maybe (Text, [S.Param])
known scope name =
    case splitQualified name of
        Just (qualifier, unqualified)
            -> Map.lookup qualifier (scopeImports scope) >>= \imported ->
                   case unqualified `Set.member` L.importedExports imported of
                       True  -> (unqualified,) <$> parametersOf (L.importedRoot imported ^. L.functionRegistry) unqualified
                       False -> Nothing
        Nothing
            -> case Map.member name (scopeLocals scope) of
                   True  -> Nothing
                   False -> (name,) <$> parametersOf (scopeFunctions scope) name

parametersOf :: L.FunctionRegistry -> Text -> Maybe [S.Param]
parametersOf registry called = Map.lookup called registry >>= \case
    SomeFunction (Function _ _ _ _ params _ _) -> Just params


-- A case branch: its condition, or its pattern's values, and its result, around which the
-- pattern's holes are bound, each a part of the target, of the size given
branch :: Scope -> Size -> S.Expr -> Found
branch scope target = \case
    S.Form _ [S.Symbol _ "otherwise", body]                   -> names scope body
    S.Form _ [S.Form _ [S.Symbol _ "%when", condition], body] -> names scope condition <> names scope body
    S.Form _ [p, body]
        | S.isPatternForm p                                   -> case pattern scope p of
                                                                     Just (found, holes) -> found <> names (sized (holeSizes p target) (within holes scope)) body
                                                                     Nothing             -> mempty
    S.Form _ [S.Literal _ _, body]                            -> names scope body
    _rejected                                                 -> mempty


-- A record's attribute pairs see each other, but never themselves (see 'frame'). Its context
-- and its inlays are read inside it; an inlay of an array literal is the evaluator's error.
record :: Scope -> [S.Expr] -> Found
record scope items = case collectBindings items of
    Left _rejected -> mempty
    Right entries  -> let inner = sized (frameSizes scope entries) (withinFrame (frameNames entries) scope)
                      in frame scope entries
                         <> mconcat (take 1 [ names inner e | S.Form _ [S.Symbol _ "%context", e] <- items ])
                         <> mconcat [ names inner e | S.Form _ inlay@[S.Symbol _ (S.formName -> "inlay"), e] <- items
                                                    , not (shadowsForm (bound inner) inlay), not (literal TargetList e) ]


-- An array's elements; an attribute pair in it is erased, and an inlay of a record literal
-- is the evaluator's error
array :: Scope -> [S.Expr] -> Found
array scope = foldMap $ \case
    S.Form _ (S.Symbol _ keyword : _)
        | isFunctionKeyword keyword                          -> mempty
    S.Form _ [S.Symbol _ "%binder", S.Literal _ (S.Str _)]   -> mempty
    S.Form _ (S.Symbol _ "context" : _)                      -> mempty
    S.Form _ (S.Symbol _ "attr" : _)                         -> mempty
    S.Form _ [S.Symbol _ "%attr", _, _]                      -> mempty
    S.Form _ inlay@[S.Symbol _ (S.formName -> "inlay"), e]
        | not (shadowsForm (bound scope) inlay), literal TargetRecord e
                                                             -> mempty
        | not (shadowsForm (bound scope) inlay)              -> names scope e
    element                                                  -> names scope element


-- The values of a frame's bindings, a record's attribute pairs or a let's bindings, each read
-- with the frame's names but its own, which names what it names around the frame. A name
-- bound twice is bound to its first binding, so only the first is hidden from itself. A
-- binding of a fn is a function of its own, whose parameters are its own.
frame :: Scope -> [FrameEntry] -> Found
frame scope entries =
    mconcat [ value (withinFrame (visible i entry) scope) entry | (i, entry) <- zip [0 :: Int ..] entries ]
    where
    bindings = frameNames entries

    visible i entry = case entryKey entry `elem` map entryKey (take i entries) of
                          True  -> bindings
                          False -> Map.delete (entryKey entry) bindings

    value scope' entry = case (Map.findWithDefault Nothing (entryKey entry) bindings, entryExpr entry) of
                             (Just node, S.Form _ items)
                                 | Just found <- lambda (inside node scope') ownParameters items -> found
                             _otherValue                                                         -> names scope' (entryExpr entry)

-- A frame's names, each bound to its first binding, with the function it names if that
-- binding is a fn
frameNames :: [FrameEntry] -> Map.Map Text (Maybe Node)
frameNames entries = Map.fromListWith (\_later first -> first) [ (entryKey entry, fnNode entry) | entry <- entries ]
    where
    fnNode entry = case entryExpr entry of
                       S.Form _ (S.Symbol _ (S.formName -> "fn") : _) -> Just (Bound (entryKey entry) (entryPos entry))
                       _notAFn                                        -> Nothing

-- The sizes of a frame's bindings, known against the function they are written in, each bound
-- to its first binding. A name the frame binds is of no known size in them, whether it names
-- a sibling or, in its own binding, the name around the frame.
frameSizes :: Scope -> [FrameEntry] -> Map.Map Text Size
frameSizes scope entries = Map.fromListWith (\_later first -> first) [ (entryKey entry, size framed (entryExpr entry)) | entry <- entries ]
    where
    framed = withinFrame (frameNames entries) scope


-- Whether an expression is a form its elements make a record or an array
literal :: BlockTarget -> S.Expr -> Bool
literal target = \case
    S.Form _ inner -> determineBlockTarget inner == target
    _notAForm      -> False
