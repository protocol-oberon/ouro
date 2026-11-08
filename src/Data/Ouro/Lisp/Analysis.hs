{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- The static analysis: every module checked as written, all of it, before anything is
-- evaluated, so a mistake in a function no graph calls, or in a branch not taken, is reported
-- too (see the org note, "Static analysis"). It checks names, that every name resolves where
-- it is written; calls, that a call of a known function has its arity; and recursion, that no
-- function reaches itself by its name. The scope rules are the evaluator's, and what a form is
-- and what it binds come from 'Analysis.Forms', which the evaluator uses too.
module Data.Ouro.Lisp.Analysis
( analyseModule
) where

import           Data.Containers.ListUtils     (nubOrd)
import           Data.Foldable                 (asum)
import           Data.Function                 ((&))
import           Data.Graph                    (SCC (..), stronglyConnComp)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Ouro.Error.Diagnostics   (incorrectArity, notExported,
                                                recursiveCall, unboundName,
                                                unknownQualifier, withBlurb)
import           Data.Ouro.Error.Types         (OuroError (..))
import           Data.Ouro.Lisp.Analysis.Forms (BlockTarget (..), FrameEntry (..),
                                                collectBindings,
                                                determineBlockTarget,
                                                hasValidOtherwise, holeName,
                                                isRestHole, isSubjectHole,
                                                letBindings, param, shadowsForm)
import           Data.Ouro.Lisp.Eval.Builtins  (builtinRegistry)
import qualified Data.Ouro.Lisp.Eval.Types     as L
import           Data.Ouro.Lisp.Function       (checkParams, functionBody,
                                                isFunctionKeyword)
import           Data.Ouro.Lisp.Module.Bundled (bundled)
import           Data.Ouro.Lisp.Module.Types   (Declaration (..),
                                                HigherExpression (..), Module,
                                                functionRegistry, graphRegistry,
                                                splitQualified)
import qualified Data.Ouro.Lisp.Surface        as S
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Text.Megaparsec               (SourcePos)


-- A function that can be reached by its name: a defn of the module, or a fn bound by name,
-- by a let binding or an attribute pair, where it is bound
data Node
    = Defn  Text
    | Bound Text SourcePos
    deriving (Eq, Ord)

nodeName :: Node -> Text
nodeName = \case
    Defn  name   -> name
    Bound name _ -> name

-- A function's name written inside another function (or itself), where it is written: a call,
-- or the function passed on as a value, as to map
data Call = Call
    { callFrom :: Node
    , callTo   :: Node
    , callPos  :: SourcePos
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
-- them all. The function being written, if any, is where a name of another function calls it.
data Scope = Scope
    { scopeLocals    :: Map.Map Text (Maybe Node)
    , scopeFunctions :: L.FunctionRegistry
    , scopeImports   :: Map.Map Text L.Imported
    , scopeOwner     :: Maybe Node
    }

-- Whether a binding around this point has the name: a local or a function of the module
bound :: Scope -> Text -> Bool
bound scope name = Map.member name (scopeLocals scope) || Map.member name (scopeFunctions scope)

-- The scope with more names bound around it, none of them a function's
within :: Set.Set Text -> Scope -> Scope
within more = withinFrame (Map.fromSet (const Nothing) more)

-- The scope with a frame's names bound around it
withinFrame :: Map.Map Text (Maybe Node) -> Scope -> Scope
withinFrame more scope = scope { scopeLocals = more <> scopeLocals scope }

-- The scope inside a function's body
inside :: Node -> Scope -> Scope
inside node scope = scope { scopeOwner = Just node }


-- Every error in a module's functions and graphs, in source order, with the modules it
-- imports, by qualifier
analyseModule :: Map.Map Text L.Imported -> Module -> [OuroError]
analyseModule imported ouroModule =
    sortOn errPos . nubOrd $ errs <> recursion calls
    where
    Found errs calls = foldMap function (Map.elems (ouroModule ^. functionRegistry))
                    <> foldMap graph    (Map.elems (ouroModule ^. graphRegistry))

    root = Scope Map.empty (ouroModule ^. functionRegistry) imported Nothing

    -- A body the parser rejected is reported by the parser
    function :: HigherExpression 'FunctionExpr -> Found
    function (Function pos name _ params body) =
        case body of
            S.Form _ exprs | Right result <- functionBody name pos exprs -> parameters (inside (Defn name) root) params result
            _rejected                                                     -> mempty

    graph :: HigherExpression 'GraphExpr -> Found
    graph (Graph _ _ body) = names root body


-- A function that reaches itself by its name, directly or through others, is recursion,
-- reported once for each circle of functions, at the name that closes it: the last written.
-- A function reached through a value, a parameter, or a function a call returns, is the
-- evaluator's to check, when it is called.
recursion :: [Call] -> [OuroError]
recursion calls =
    [ OuroError (callPos closing) (recursiveCall (map nodeName (route next (callTo closing) (callFrom closing) <> [callTo closing])))
    | CyclicSCC circle <- stronglyConnComp [ (node, node, Map.findWithDefault [] node next) | node <- Map.keys next ]
    , let members = Set.fromList circle
          within' = [ call | call <- calls, callFrom call `Set.member` members, callTo call `Set.member` members ]
    , closing : _ <- [reverse (sortOn callPos within')]
    ]
    where
    next = Map.fromListWith (<>) ([ (callFrom call, [callTo call]) | call <- calls ] <> [ (callTo call, []) | call <- calls ])


-- The functions from one to another, both included, following calls
route :: Map.Map Node [Node] -> Node -> Node -> [Node]
route next start goal = fromMaybe [start, goal] (search Set.empty start)
    where
    search seen node
        | node == goal             = Just [goal]
        | node `Set.member` seen   = Nothing
        | otherwise                = (node :) <$> asum (map (search (Set.insert node seen)) (Map.findWithDefault [] node next))


-- A function's parameters, and the holes of their patterns, are bound around its body. The
-- values in a pattern are read in the scope the function is written in, which cannot see
-- its parameters. A malformed pattern stops the call before the body, so the body is not
-- looked into.
parameters :: Scope -> [S.Param] -> S.Expr -> Found
parameters closure params body =
    case mconcat <$> traverse (pattern closure) [ p | Just p <- map S.paramPattern params ] of
        Nothing                    -> mempty
        Just (patternFound, holes) -> patternFound <> names (within (Set.fromList (map S.paramName params) <> holes) closure) body


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


-- Every name in an expression resolves where it is written
names :: Scope -> S.Expr -> Found
names scope = \case
    S.Symbol pos name  -> resolve scope pos name
    S.Tagged _ _ inner -> names scope inner
    S.Form pos items   -> form scope pos items
    _leaf              -> mempty


-- A name: one bound around it, a function of the module, or a built-in; or, qualified, a
-- function an import exports. Unbound, it is SCP0000, with the nearest name in scope, the
-- names Canon gives its own bindings ('%target', '%1') aside. A function's name, written in
-- a function, is a call of it (an import's cannot call back, since imports have no cycles).
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
    | Just local <- Map.lookup name (scopeLocals scope)
        = foldMap called local
    | Map.member name (scopeFunctions scope)
        = called (Defn name)
    | Map.member name builtinRegistry
        = mempty
    | otherwise
        = problem (OuroError pos (unboundName name [ n | n <- inScope, not ("%" `T.isPrefixOf` n) ]))
    where
    inScope = Map.keys (scopeLocals scope) <> Map.keys (scopeFunctions scope)

    called node = case scopeOwner scope of
                      Just owner -> Found [] [Call owner node pos]
                      Nothing    -> mempty


-- A form, read as the evaluator reads it (see 'Engine.evalExpr'). A form the evaluator rejects
-- whole, without evaluating what is in it, is not looked into: its own error is the
-- evaluator's, where it is evaluated.
form :: Scope -> SourcePos -> [S.Expr] -> Found
form scope pos items = case items of
    -- A special form's name that a binding around the form has is the binding's, as in Scheme:
    -- the form is an ordinary call of it
    _shadowed
        | shadowsForm (bound scope) items             -> arity scope pos items <> foldMap (names scope) items

    S.Symbol _ "context" : _                         -> mempty
    S.Symbol _ "attr" : _                            -> mempty

    -- A path's keys are not names
    S.Symbol _ "get" : target : _path                -> names scope target
    S.Symbol _ "has" : target : _ : _                -> names scope target
    S.Symbol _ "has" : _                             -> mempty
    S.Symbol _ "get-or" : fallback : target : _ : _  -> names scope fallback <> names scope target
    S.Symbol _ "get-or" : _                          -> mempty
    S.Symbol _ "image" : target : _ : _              -> names scope target
    S.Symbol _ "image" : _                           -> mempty

    S.Symbol _ "list" : elements                     -> foldMap (names scope) elements
    [S.Symbol _ "map", f, xs]                        -> names scope f <> names scope xs
    S.Symbol _ "map" : _                             -> mempty
    [S.Symbol _ quantifier, f, xs]
        | quantifier `elem` quantifiers              -> names scope f <> names scope xs
    S.Symbol _ quantifier : _
        | quantifier `elem` quantifiers              -> mempty
    [S.Symbol _ "%apply", f, x]                      -> names scope f <> names scope x
    [S.Symbol _ "->"]                                -> mempty
    [S.Symbol _ "with"]                              -> mempty
    [S.Symbol _ "adjoin"]                            -> mempty

    S.Symbol _ (S.formName -> "let") : [S.Form _ bindings, body]
        | Right entries <- letBindings bindings      -> frame scope entries <> names (withinFrame (frameNames entries) scope) body
    S.Symbol _ (S.formName -> "let") : _             -> mempty

    S.Symbol _ "when" : _                            -> mempty
    [S.Symbol _ "satisfies", S.Tagged _ _ subject]
        | not (isSubjectHole subject)                -> names scope subject
    S.Symbol _ "satisfies" : _                       -> mempty
    S.Symbol _ keyword : _
        | isFunctionKeyword keyword                  -> mempty
    [S.Symbol _ "%binder", S.Literal _ (S.Str _)]    -> mempty

    [S.Symbol fnPos (S.formName -> "fn"), S.Form _ params, body]
        | Just typed <- traverse param (zip [1 ..] params)
                                                     -> lambda fnPos typed body
    [S.Symbol fnPos (S.formName -> "fn"), S.Tagged _ _ (S.Form _ params), body]
        | Just typed <- traverse param (zip [1 ..] params)
                                                     -> lambda fnPos typed body
    S.Symbol _ (S.formName -> "fn") : _              -> mempty

    [S.Symbol _ "%attr", S.Literal _ (S.Str _), value]
                                                     -> names scope value

    S.Symbol _ (S.formName -> "case") : target : branches
        | hasValidOtherwise branches                 -> names scope target <> foldMap (branch scope) branches
        | otherwise                                  -> mempty

    S.Symbol _ "nth" : _                             -> mempty
    [S.Symbol _ op, a, b, c]
        | op `elem` ["insert", "replace-at"]         -> foldMap (names scope) [a, b, c]

    -- Anything else is what its elements make it. A call's head is a name like any other.
    _otherForm -> case determineBlockTarget items of
                      TargetRecord      -> record scope items
                      TargetList        -> array scope items
                      TargetFunctionApp -> arity scope pos items <> foldMap (names scope) items

    where
    quantifiers = ["exists", "forall", "omits", "filter"]

    -- A fn's parameters are checked when it is written; one the evaluator rejects is its error
    lambda fnPos typed body = case checkParams fnPos typed of
                                  Right () -> parameters scope typed body
                                  Left _   -> mempty


-- A call of a known function has as many arguments as the function has parameters (SCP0002),
-- reported at the call with how the function is called, as the evaluator reports it. Only a
-- call written with the function's name is known here: a function reached through a value, a
-- parameter, a let binding or map, is checked when it is called.
arity :: Scope -> SourcePos -> [S.Expr] -> Found
arity scope pos = \case
    S.Symbol _ name : args
        | Just (called, params) <- known scope name
        , length params /= length args
        -> incorrectArity called (length params) (length args)
           & maybe id (\call -> withBlurb ("It is called " <> call <> ".")) (L.callTemplate called params)
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
    Function _ _ _ params _ -> Just params


-- A case branch: its condition, or its pattern's values, and its result, around which the
-- pattern's holes are bound
branch :: Scope -> S.Expr -> Found
branch scope = \case
    S.Form _ [S.Symbol _ "otherwise", body]                   -> names scope body
    S.Form _ [S.Form _ [S.Symbol _ "%when", condition], body] -> names scope condition <> names scope body
    S.Form _ [p, body]
        | S.isPatternForm p                                   -> case pattern scope p of
                                                                     Just (found, holes) -> found <> names (within holes scope) body
                                                                     Nothing             -> mempty
    S.Form _ [S.Literal _ _, body]                            -> names scope body
    _rejected                                                 -> mempty


-- A record's attribute pairs see each other, but never themselves (see 'frame'). Its context
-- and its inlays are read inside it; an inlay of an array literal is the evaluator's error.
record :: Scope -> [S.Expr] -> Found
record scope items = case collectBindings items of
    Left _rejected -> mempty
    Right entries  -> let inner = withinFrame (frameNames entries) scope
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
-- binding of a fn is a function of its own, whose body calls what it names.
frame :: Scope -> [FrameEntry] -> Found
frame scope entries =
    mconcat [ names (owned (withinFrame (visible i entry) scope)) (entryExpr entry)
            | (i, entry) <- zip [0 :: Int ..] entries
            , let owned = maybe id inside (Map.findWithDefault Nothing (entryKey entry) bindings) ]
    where
    bindings = frameNames entries

    visible i entry = case entryKey entry `elem` map entryKey (take i entries) of
                          True  -> bindings
                          False -> Map.delete (entryKey entry) bindings

-- A frame's names, each bound to its first binding, with the function it names if that
-- binding is a fn
frameNames :: [FrameEntry] -> Map.Map Text (Maybe Node)
frameNames entries = Map.fromListWith (\_later first -> first) [ (entryKey entry, fnNode entry) | entry <- entries ]
    where
    fnNode entry = case entryExpr entry of
                       S.Form _ (S.Symbol _ (S.formName -> "fn") : _) -> Just (Bound (entryKey entry) (entryPos entry))
                       _notAFn                                        -> Nothing


-- Whether an expression is a form its elements make a record or an array
literal :: BlockTarget -> S.Expr -> Bool
literal target = \case
    S.Form _ inner -> determineBlockTarget inner == target
    _notAForm      -> False
