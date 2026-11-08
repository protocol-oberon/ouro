{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- The static analysis: every module checked as written, all of it, before anything is
-- evaluated, so a mistake in a function no graph calls, or in a branch not taken, is reported
-- too (see the org note, "Static analysis"). It checks names, that every name resolves where
-- it is written, and calls, that a call of a known function has its arity. The scope rules are
-- the evaluator's, and what a form is and what it binds come from 'Analysis.Forms', which the
-- evaluator uses too.
module Data.Ouro.Lisp.Analysis
( analyseModule
) where

import           Data.Containers.ListUtils     (nubOrd)
import           Data.Function                 ((&))
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Ouro.Error.Diagnostics   (incorrectArity, notExported,
                                                unboundName, unknownQualifier,
                                                withBlurb)
import           Data.Ouro.Error.Types         (OuroError (..))
import           Data.Ouro.Lisp.Analysis.Forms (BlockTarget (..), FrameEntry (..),
                                                collectBindings,
                                                determineBlockTarget,
                                                hasValidOtherwise, holeName,
                                                isRestHole, isSubjectHole,
                                                letBindings, param)
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


-- What a name can refer to where it is written: the names bound around it (attribute pairs,
-- let bindings, parameters, holes), the module's own functions, and the modules it imports, by
-- qualifier. The built-ins come after them all.
data Scope = Scope
    { scopeLocals    :: Set.Set Text
    , scopeFunctions :: L.FunctionRegistry
    , scopeImports   :: Map.Map Text L.Imported
    }

-- The scope with more names bound around it
within :: Set.Set Text -> Scope -> Scope
within bound scope = scope { scopeLocals = bound <> scopeLocals scope }


-- Every error in a module's functions and graphs, in source order, with the modules it
-- imports, by qualifier
analyseModule :: Map.Map Text L.Imported -> Module -> [OuroError]
analyseModule imported ouroModule =
    sortOn errPos . nubOrd $ concatMap function (Map.elems (ouroModule ^. functionRegistry))
                          <> concatMap graph    (Map.elems (ouroModule ^. graphRegistry))
    where
    root = Scope Set.empty (ouroModule ^. functionRegistry) imported

    -- A body the parser rejected is reported by the parser
    function :: HigherExpression 'FunctionExpr -> [OuroError]
    function (Function pos name _ params body) =
        case body of
            S.Form _ exprs | Right result <- functionBody name pos exprs -> parameters root params result
            _rejected                                                     -> []

    graph :: HigherExpression 'GraphExpr -> [OuroError]
    graph (Graph _ _ body) = names root body


-- A function's parameters, and the holes of their patterns, are bound around its body. The
-- values in a pattern are read in the scope the function is written in, which cannot see
-- its parameters. A malformed pattern stops the call before the body, so the body is not
-- looked into.
parameters :: Scope -> [S.Param] -> S.Expr -> [OuroError]
parameters closure params body =
    case mconcat <$> traverse (pattern closure) [ p | Just p <- map S.paramPattern params ] of
        Nothing                     -> []
        Just (patternErrors, holes) -> patternErrors <> names (within (Set.fromList (map S.paramName params) <> holes) closure) body


-- A case or parameter pattern, as 'matchPattern' reads it: the errors in the values it
-- compares by, and the names its holes bind; or Nothing when it is malformed, which is the
-- evaluator's error: a hole inside a value, a Record pattern with anything but ':key pattern'
-- entries and one '?..rest', or a '?..rest' before the end of an Array pattern.
pattern :: Scope -> S.Expr -> Maybe ([OuroError], Set.Set Text)
pattern scope = \case
    S.Hole _ hole
        -> Just ([], bound hole)

    S.Form _ elems
        | any S.isAttrForm elems
        -> let keyed  = [ p | S.Form _ [S.Symbol _ "%attr", S.Literal _ (S.Str _), p] <- elems ]
               rests  = [ hole | S.Hole _ hole <- elems, isRestHole hole ]
               strays = length elems - length keyed - length rests
           in case (strays, rests) of
                  (0, [])     -> mconcat <$> traverse (pattern scope) keyed
                  (0, [rest]) -> (<> ([], bound rest)) . mconcat <$> traverse (pattern scope) keyed
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
    bound hole = foldMap Set.singleton (holeName hole)

    byValue p = case S.containsHole p of
                    True  -> Nothing
                    False -> Just (names scope p, Set.empty)


-- Every name in an expression resolves where it is written
names :: Scope -> S.Expr -> [OuroError]
names scope = \case
    S.Symbol pos name  -> resolve scope pos name
    S.Tagged _ _ inner -> names scope inner
    S.Form pos items   -> form scope pos items
    _leaf              -> []


-- A name: one bound around it, a function of the module, or a built-in; or, qualified, a
-- function an import exports. Unbound, it is SCP0000, with the nearest name in scope, the
-- names Canon gives its own bindings ('%target', '%1') aside.
resolve :: Scope -> SourcePos -> Text -> [OuroError]
resolve scope pos name
    | Just (qualifier, unqualified) <- splitQualified name
        = case Map.lookup qualifier (scopeImports scope) of
              Nothing
                  -> [OuroError pos (unknownQualifier qualifier (Map.keys (scopeImports scope)) (Map.keys bundled))]
              Just imported
                  | unqualified `Set.member` exported -> []
                  | otherwise                         -> [OuroError pos (notExported qualifier unqualified (Set.toList exported))]
                  where
                  exported = L.importedExports imported
    | name `Set.member` scopeLocals scope || Map.member name (scopeFunctions scope) || Map.member name builtinRegistry
        = []
    | otherwise
        = [OuroError pos (unboundName name [ n | n <- inScope, not ("%" `T.isPrefixOf` n) ])]
    where
    inScope = Set.toList (scopeLocals scope) <> Map.keys (scopeFunctions scope)


-- A form, read as the evaluator reads it (see 'Engine.evalExpr'). A form the evaluator rejects
-- whole, without evaluating what is in it, is not looked into: its own error is the
-- evaluator's, where it is evaluated.
form :: Scope -> SourcePos -> [S.Expr] -> [OuroError]
form scope pos items = case items of
    S.Symbol _ "context" : _                         -> []
    S.Symbol _ "attr" : _                            -> []

    -- A path's keys are not names
    S.Symbol _ "get" : target : _path                -> names scope target
    S.Symbol _ "has" : target : _ : _                -> names scope target
    S.Symbol _ "has" : _                             -> []
    S.Symbol _ "get-or" : fallback : target : _ : _  -> names scope fallback <> names scope target
    S.Symbol _ "get-or" : _                          -> []
    S.Symbol _ "image" : target : _ : _              -> names scope target
    S.Symbol _ "image" : _                           -> []

    S.Symbol _ "list" : elements                     -> concatMap (names scope) elements
    [S.Symbol _ "map", f, xs]                        -> names scope f <> names scope xs
    S.Symbol _ "map" : _                             -> []
    [S.Symbol _ quantifier, f, xs]
        | quantifier `elem` quantifiers              -> names scope f <> names scope xs
    S.Symbol _ quantifier : _
        | quantifier `elem` quantifiers              -> []
    [S.Symbol _ "%apply", f, x]                      -> names scope f <> names scope x
    [S.Symbol _ "->"]                                -> []
    [S.Symbol _ "with"]                              -> []
    [S.Symbol _ "adjoin"]                            -> []

    S.Symbol _ "let" : [S.Form _ bindings, body]
        | Right entries <- letBindings bindings      -> frame scope entries <> names (within (keys entries) scope) body
    S.Symbol _ "let" : _                             -> []

    S.Symbol _ "define" : _                          -> []
    S.Symbol _ "when" : _                            -> []
    [S.Symbol _ "satisfies", S.Tagged _ _ subject]
        | not (isSubjectHole subject)                -> names scope subject
    S.Symbol _ "satisfies" : _                       -> []
    S.Symbol _ keyword : _
        | isFunctionKeyword keyword                  -> []
    [S.Symbol _ "%binder", S.Literal _ (S.Str _)]    -> []

    [S.Symbol fnPos "fn", S.Form _ params, body]
        | Just typed <- traverse param (zip [1 ..] params)
                                                     -> lambda fnPos typed body
    [S.Symbol fnPos "fn", S.Tagged _ _ (S.Form _ params), body]
        | Just typed <- traverse param (zip [1 ..] params)
                                                     -> lambda fnPos typed body
    S.Symbol _ "fn" : _                              -> []

    [S.Symbol _ "%attr", S.Literal _ (S.Str _), value]
                                                     -> names scope value

    S.Symbol _ "case" : target : branches
        | hasValidOtherwise branches                 -> names scope target <> concatMap (branch scope) branches
        | otherwise                                  -> []

    S.Symbol _ "nth" : _                             -> []
    [S.Symbol _ op, a, b, c]
        | op `elem` ["insert", "replace-at"]         -> concatMap (names scope) [a, b, c]

    -- Anything else is what its elements make it. A call's head is a name like any other.
    _otherForm -> case determineBlockTarget items of
                      TargetRecord      -> record scope items
                      TargetList        -> array scope items
                      TargetFunctionApp -> arity scope pos items <> concatMap (names scope) items

    where
    quantifiers = ["exists", "forall", "omits", "filter"]

    -- A fn's parameters are checked when it is written; one the evaluator rejects is its error
    lambda fnPos typed body = case checkParams fnPos typed of
                                  Right () -> parameters scope typed body
                                  Left _   -> []


-- A call of a known function has as many arguments as the function has parameters (SCP0002),
-- reported at the call with how the function is called, as the evaluator reports it. Only a
-- call written with the function's name is known here: a function reached through a value, a
-- parameter, a let binding or map, is checked when it is called.
arity :: Scope -> SourcePos -> [S.Expr] -> [OuroError]
arity scope pos = \case
    S.Symbol _ name : args
        | Just (called, params) <- known scope name
        , length params /= length args
        -> [ incorrectArity called (length params) (length args)
             & maybe id (\call -> withBlurb ("It is called " <> call <> ".")) (L.callTemplate called params)
             & OuroError pos ]
    _otherCall
        -> []


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
            -> case name `Set.member` scopeLocals scope of
                   True  -> Nothing
                   False -> (name,) <$> parametersOf (scopeFunctions scope) name

parametersOf :: L.FunctionRegistry -> Text -> Maybe [S.Param]
parametersOf registry called = Map.lookup called registry >>= \case
    Function _ _ _ params _ -> Just params


-- A case branch: its condition, or its pattern's values, and its result, around which the
-- pattern's holes are bound
branch :: Scope -> S.Expr -> [OuroError]
branch scope = \case
    S.Form _ [S.Symbol _ "otherwise", body]                   -> names scope body
    S.Form _ [S.Form _ [S.Symbol _ "%when", condition], body] -> names scope condition <> names scope body
    S.Form _ [p, body]
        | S.isPatternForm p                                   -> case pattern scope p of
                                                                     Just (errs, holes) -> errs <> names (within holes scope) body
                                                                     Nothing            -> []
    S.Form _ [S.Literal _ _, body]                            -> names scope body
    _rejected                                                 -> []


-- A record's attribute pairs see each other, but never themselves (see 'frame'). Its context
-- and its inlays are read inside it; an inlay of an array literal is the evaluator's error.
record :: Scope -> [S.Expr] -> [OuroError]
record scope items = case collectBindings items of
    Left _rejected -> []
    Right entries  -> let inside = within (keys entries) scope
                      in frame scope entries
                         <> concat (take 1 [ names inside e | S.Form _ [S.Symbol _ "%context", e] <- items ])
                         <> concat [ names inside e | S.Form _ [S.Symbol _ "inlay", e] <- items, not (literal TargetList e) ]


-- An array's elements; an attribute pair in it is erased, and an inlay of a record literal
-- is the evaluator's error
array :: Scope -> [S.Expr] -> [OuroError]
array scope = concatMap $ \case
    S.Form _ (S.Symbol _ "define" : _)                       -> []
    S.Form _ (S.Symbol _ keyword : _)
        | isFunctionKeyword keyword                          -> []
    S.Form _ [S.Symbol _ "%binder", S.Literal _ (S.Str _)]   -> []
    S.Form _ (S.Symbol _ "context" : _)                      -> []
    S.Form _ (S.Symbol _ "attr" : _)                         -> []
    S.Form _ [S.Symbol _ "%attr", _, _]                      -> []
    S.Form _ [S.Symbol _ "inlay", e]
        | literal TargetRecord e                             -> []
        | otherwise                                          -> names scope e
    element                                                  -> names scope element


-- The values of a frame's bindings, a record's attribute pairs or a let's bindings, each read
-- with the frame's names but its own, which names what it names around the frame. A name
-- bound twice is bound to its first binding, so only the first is hidden from itself.
frame :: Scope -> [FrameEntry] -> [OuroError]
frame scope entries =
    concat [ names (within (visible i entry) scope) (entryExpr entry) | (i, entry) <- zip [0 :: Int ..] entries ]
    where
    visible i entry = case entryKey entry `elem` map entryKey (take i entries) of
                          True  -> keys entries
                          False -> Set.delete (entryKey entry) (keys entries)

keys :: [FrameEntry] -> Set.Set Text
keys = Set.fromList . map entryKey


-- Whether an expression is a form its elements make a record or an array
literal :: BlockTarget -> S.Expr -> Bool
literal target = \case
    S.Form _ inner -> determineBlockTarget inner == target
    _notAForm      -> False
