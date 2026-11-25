{-# LANGUAGE GADTs #-}

module Data.Ouro.Refinement.Check where

import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.Ouro.Error.Diagnostics (requirementFailed, typeMismatchBlurb, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..), TypeError (..))
import qualified Data.Ouro.Internal.Expr     as I
import           Data.Ouro.Internal.Uri      (mkUri, uriParsed)
import           Data.Ouro.Lisp.Eval.Types   (EvalM, Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types   as L
import qualified Data.Ouro.Refinement.Requirement as Q
import qualified Data.Ouro.Refinement.Type   as R
import           Data.Ouro.Refinement.Value  (hasKind, refinementOf)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Megaparsec             (SourcePos)
import qualified Text.URI                    as URI


-- How a refinement defined in Ouro is run on a value, which only the evaluator can do: the
-- refinement by its name, called where its type is written, answering true or false with its
-- doc, or the error the call is (no such refinement, a procedure, a body that fails). The
-- checker takes it as an argument, so that checking never depends on the evaluator: layer 1
-- meets layer 0 here, and only here.
type RunDefined = SourcePos -> Text -> L.Expr -> EvalM (Either L.Expr (Bool, Maybe Text))


-- Checks an evaluated value against a type, written at the first position, the value at the
-- second: the value itself, never converted, or a RefinementMismatch error at the value.
-- A type is a refinement by its name, or one built from them with ∪ ∩ ∖, (array p) and
-- (record :k p …).
checkRefinement :: RunDefined -> R.Type -> SourcePos -> SourcePos -> L.Expr -> EvalM L.Expr
checkRefinement run ty typePos pos evaluatedVal = case ty of
    R.Named refinement
        -> case refinement of
               R.RDefined name -> checkDefined run typePos pos name evaluatedVal
               builtIn         -> case builtInProblem builtIn evaluatedVal of
                                      Nothing              -> pure evaluatedVal
                                      Just (actual, blurb) -> failRefinement (R.Named builtIn) actual blurb

    -- ∪: in a or in b: b is asked only when the value is outside a
    R.Union a b -> inType a >>= \case
        Right True  -> pure evaluatedVal
        Right False -> inType b >>= passesIf True
        Left err    -> pure err

    -- ∩: in a and in b: b is asked only when the value is in a
    R.Inter a b -> inType a >>= \case
        Right True  -> inType b >>= passesIf True
        Right False -> notIn
        Left err    -> pure err

    -- ∖: in a and not in b: it holds only when b answers that the value is outside it
    R.Minus a b -> inType a >>= \case
        Right True  -> inType b >>= passesIf False
        Right False -> notIn
        Left err    -> pure err

    -- (array p): an array whose every element is in p: the elements are asked in order, and
    -- the first outside is named
    R.ArrayOf element
        -> case L.asArray evaluatedVal of
               Nothing    -> notIn
               Just items -> everyElement element (zip [1 :: Int ..] items)

    -- (record :k p …): a record with at least these keys, each value in its type: the keys
    -- are asked in the order written, and the first missing or outside is named
    R.RecordOf pairs
        -> case L.asRecord evaluatedVal of
               Nothing     -> notIn
               Just fields -> everyKey fields pairs

    where
    -- Whether the value is in a type: True, False where the check is a refinement mismatch, or
    -- the error any other failure is, which stays that error, so a refinement broken inside a
    -- union is never taken for a value outside it
    inType :: R.Type -> EvalM (Either L.Expr Bool)
    inType part = inTypeOf part evaluatedVal

    -- The same, of another value: an element of an array, or a record's value at a key
    inTypeOf :: R.Type -> L.Expr -> EvalM (Either L.Expr Bool)
    inTypeOf part value = checkRefinement run part typePos pos value
        <&> \case
             EvalError (OuroError _ ctx) | isRefinementMismatch ctx
                 -> Right False

             err@(EvalError _)
                 -> Left err

             _passed
                 -> Right True

    -- The value, if the last answer is the one wanted; the whole type's mismatch if not
    passesIf :: Bool -> Either L.Expr Bool -> EvalM L.Expr
    passesIf wanted = \case
        Right answer | answer == wanted -> pure evaluatedVal
        Right _                         -> notIn
        Left err                        -> pure err

    -- A value outside a compound type is reported against the whole of it
    notIn :: EvalM L.Expr
    notIn = failRefinement ty (refinementOf evaluatedVal) ""

    -- Each element in the element type, in order, stopping at the first outside it; an error
    -- inside an element's check stays that error
    everyElement :: R.Type -> [(Int, L.Expr)] -> EvalM L.Expr
    everyElement element = \case
        []               -> pure evaluatedVal
        (i, item) : rest -> inTypeOf element item >>= \case
            Right True  -> everyElement element rest
            Right False -> failRefinement ty ("an array whose element " <> T.pack (show i) <> " is " <> refinementOf item) ""
            Left err    -> pure err

    -- Each written key present, and its value in the key's type, in order, stopping at the
    -- first missing or outside; an error inside a key's check stays that error
    everyKey :: [(Text, L.Expr)] -> [(Text, R.Type)] -> EvalM L.Expr
    everyKey fields = \case
        []
            -> pure evaluatedVal

        (key, keyType) : rest
            -> case lookup key fields of
                   Nothing    -> failRefinement ty ("a record without " <> key) ""
                   Just value -> inTypeOf keyType value >>= \case
                       Right True  -> everyKey fields rest
                       Right False -> failRefinement ty ("a record whose " <> key <> " is " <> refinementOf value) ""
                       Left err    -> pure err

    -- A value without the refinement it is checked against, at the value
    failRefinement :: R.Type -> Text -> Text -> EvalM L.Expr
    failRefinement required actual blurb = pure (refinementFailure pos (failureOf required typePos actual Nothing) blurb)


-- A refinement defined in Ouro, titled: the value has it if the refinement, run on the value,
-- answers true. Any error running it, its parameter's requirement among them, stays the
-- error; a false answer is the value outside it, with the refinement's doc as the rule.
checkDefined :: RunDefined -> SourcePos -> SourcePos -> Text -> L.Expr -> EvalM L.Expr
checkDefined run typePos pos name v = run typePos name v <&> \case
    Right (True, _)    -> v
    Right (False, doc) -> refinementFailure pos (failureOf (R.Named (R.RDefined name)) typePos given doc) (rule doc)
    Left err           -> err

    where
    given = "a " <> refinementOf v <> " that is not " <> name

    -- The refinement's doc, stated as the rule it asserts
    rule = \case
        Just text -> name <> ": " <> text
        Nothing   -> name <> " has no doc, so the rule it asserts cannot be shown. "
                  <> "Perhaps write one: (define/refinement " <> name <> " (<r ⊢ record>) (doc \"...\") ...)."


-- A value outside a requirement, at the value: the failure as data, the text the printer shows
-- read off it, and how to put it right
refinementFailure :: SourcePos -> Q.Failure -> Text -> L.Expr
refinementFailure pos failure blurb =
    requirementFailed (R.renderType (Q.required failure)) (Q.given failure) failure
    & withBlurb blurb
    & OuroError pos
    & EvalError


-- The failure of a requirement where it is checked: the type, where it is written, the value as
-- given, and the refinement's doc. The role and the trail are filled in as the check is reached.
failureOf :: R.Type -> SourcePos -> Text -> Maybe Text -> Q.Failure
failureOf required at given doc = Q.Failure
    { Q.required   = required
    , Q.requiredAt = at
    , Q.given      = given
    , Q.role       = Q.Assertion
    , Q.trail      = []
    , Q.rule       = doc
    }


-- Why a value is outside a built-in refinement, as what it is and how to put it right, or
-- Nothing when it is in it: a kind by the value's shape, uri and date by parsing a string,
-- which passes on as itself, never converted. A refinement defined in Ouro is asked by
-- calling it, never here.
builtInProblem :: R.Refinement -> L.Expr -> Maybe (Text, Text)
builtInProblem refinement value = case (refinement, value) of
    (R.RUri, Primitive (I.String rawText))
        -> case mkUri rawText of
               Left _    -> Just ("a string that is not a URI", uriBlurb)
               Right uri -> case URI.uriScheme (uriParsed uri) of
                                Just _  -> Nothing
                                Nothing -> Just ("a string without a URI scheme", uriBlurb)

    (R.RUri, _)
        -> Just (refinementOf value, uriBlurb)

    (R.RDate, Primitive (I.String rawText))
        -> case I.parseDate (T.unpack rawText) of
               Just _  -> Nothing
               Nothing -> Just ("a string that is not a date", dateBlurb)

    (R.RDate, _)
        -> Just (refinementOf value, dateBlurb)

    (kind, _)
        | hasKind kind value -> Nothing
        | otherwise          -> Just (refinementOf value, typeMismatchBlurb value)


uriBlurb, dateBlurb :: Text
uriBlurb  = "A uri is a string with a scheme, such as \"https://linked.art/example/1\"."
dateBlurb = "A date is a string in ISO-8601 form, YYYY-MM-DDTHH:mm:ssZ, such as \"1642-01-01T00:00:00Z\"."


-- Whether an error is a value lacking a refinement, under any blurbs written on it
isRefinementMismatch :: ErrorContext -> Bool
isRefinementMismatch = \case
    Typing RefinementMismatch {} -> True
    AnnotatedContext _ ctx     -> isRefinementMismatch ctx
    _otherError                -> False
