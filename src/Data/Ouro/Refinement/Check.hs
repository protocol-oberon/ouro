{-# LANGUAGE GADTs #-}

module Data.Ouro.Refinement.Check where

import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (fromMaybe)
import           Data.Ouro.Error.Diagnostics      (requirementFailed, typeMismatchHint, withBlurb)
import           Data.Ouro.Error.Types            (ErrorContext (..), OuroError (..), TypeError (..))
import           Data.Ouro.Internal.Duration      (Duration (negative), parseDuration)
import qualified Data.Ouro.Internal.Expr          as I
import           Data.Ouro.Internal.Uri           (mkUri, uriParsed)
import           Data.Ouro.Lisp.Eval.Types        (EvalM, Expr (..))
import qualified Data.Ouro.Lisp.Eval.Types        as L
import qualified Data.Ouro.Refinement.Requirement as Q
import qualified Data.Ouro.Refinement.Type        as R
import           Data.Ouro.Refinement.Value       (aValue, hasKind, kindOf, refinementOf, refinementTree)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.Megaparsec                  (SourcePos)
import qualified Text.URI                         as URI


-- What only the evaluator can do with a refinement defined in Ouro: name its domain, the type
-- its subject is declared with and where, and run it on a value, called where its type is
-- written, answering true or false with its doc, or the error the call is (no such refinement,
-- a procedure, a body that fails). The checker is given it, so that checking never depends on
-- the evaluator: layer 1 meets layer 0 here, and only here.
data Evaluator = Evaluator
    { domainOf   :: Text -> EvalM (Maybe (SourcePos, R.Type))
    , runDefined :: SourcePos -> Text -> L.Expr -> EvalM (Either L.Expr (Bool, Maybe Text))
    }


-- Checks an evaluated value against a type, required in a role, the type written at the first
-- position, the value at the second: the value itself, never converted, or a RefinementFailed
-- error at the value, its failure carrying the role. A type is a refinement by its name, or one
-- built from them with ∪ ∩ ∖, (array p) and (record :k p …).
checkRefinement :: Evaluator -> Q.Role -> R.Type -> SourcePos -> SourcePos -> L.Expr -> EvalM L.Expr
checkRefinement ev role ty typePos pos evaluatedVal = case ty of
    R.Named refinement
        -> case refinement of
               R.RDefined name -> checkDefined ev role typePos pos name evaluatedVal
               builtIn         -> case builtInProblem builtIn evaluatedVal of
                                      Nothing              -> pure evaluatedVal
                                      Just (actual, blurb) -> pure (refinementFailure pos (failureOf role (R.Named builtIn) typePos evaluatedVal actual Nothing) blurb)

    -- ∪: in a or in b: b is asked only when the value is outside a. Outside both, no one part
    -- failed, so the trail stops at the union
    R.Union a b -> inType a >>= \case
        In       -> pure evaluatedVal
        Out _    -> inType b >>= \case
                        In         -> pure evaluatedVal
                        Out _      -> notIn []
                        Broken err -> pure err
        Broken err -> pure err

    -- ∩: in a and in b: b is asked only when the value is in a, and the operand it is outside
    -- is the step the trail takes
    R.Inter a b -> inType a >>= \case
        In         -> inType b >>= \case
                          In         -> pure evaluatedVal
                          Out below  -> notIn (Q.Step typePos Q.Whole b : below)
                          Broken err -> pure err
        Out below  -> notIn (Q.Step typePos Q.Whole a : below)
        Broken err -> pure err

    -- ∖: in a and not in b: it holds only when b answers that the value is outside it. In b,
    -- the value failed nothing below, so the trail stops at the difference
    R.Minus a b -> inType a >>= \case
        In         -> inType b >>= \case
                          In         -> notIn []
                          Out _      -> pure evaluatedVal
                          Broken err -> pure err
        Out below  -> notIn (Q.Step typePos Q.Whole a : below)
        Broken err -> pure err

    -- (array p): an array whose every element is in p: the elements are asked in order, and
    -- the first outside is named
    R.ArrayOf element
        -> case L.asArray evaluatedVal of
               Nothing    -> notIn []
               Just items -> everyElement element (zip [1 :: Int ..] items)

    -- (record :k p …): a record with at least these keys, each value in its type: the keys
    -- are asked in the order written, and the first missing or outside is named
    R.RecordOf pairs
        -> case L.asRecord evaluatedVal of
               Nothing     -> notIn []
               Just fields -> everyKey fields pairs

    where
    -- What the value's check against a part of the type answered
    inType :: R.Type -> EvalM Answer
    inType part = inTypeOf part evaluatedVal

    -- The same, of another value: an element of an array, or a record's value at a key. Outside,
    -- it keeps the trail below the part, so the whole type's failure can continue it; any other
    -- error stays that error, a refinement broken inside its body among them, so a broken
    -- refinement inside a union is never taken for a value outside it
    inTypeOf :: R.Type -> L.Expr -> EvalM Answer
    inTypeOf part value = checkRefinement ev role part typePos pos value
        <&> \case
             EvalError (OuroError _ ctx) | isOutside ctx
                 -> Out (maybe [] Q.trail (failureIn ctx))

             err@(EvalError _)
                 -> Broken err

             _passed
                 -> In

    -- A value outside a compound type is reported against the whole of it, with the trail
    -- down to the part that failed
    notIn :: [Q.Step] -> EvalM L.Expr
    notIn below = failRefinement ty (refinementOf evaluatedVal) below

    -- Each element in the element type, in order, stopping at the first outside it; an error
    -- inside an element's check stays that error
    everyElement :: R.Type -> [(Int, L.Expr)] -> EvalM L.Expr
    everyElement element = \case
        []               -> pure evaluatedVal
        (i, item) : rest -> inTypeOf element item >>= \case
            In         -> everyElement element rest
            Out below  -> failRefinement ty ("an array whose element " <> T.pack (show i) <> " is " <> refinementOf item)
                                         (Q.Step typePos (Q.Element i) element : below)
            Broken err -> pure err

    -- Each written key present, and its value in the key's type, in order, stopping at the
    -- first missing or outside; an error inside a key's check stays that error
    everyKey :: [(Text, L.Expr)] -> [(Text, R.Type)] -> EvalM L.Expr
    everyKey fields = \case
        []
            -> pure evaluatedVal

        (key, keyType) : rest
            -> case lookup key fields of
                   Nothing    -> failRefinement ty ("a record without " <> key) [Q.Step typePos (Q.Missing key) keyType]
                   Just value -> inTypeOf keyType value >>= \case
                       In         -> everyKey fields rest
                       Out below  -> failRefinement ty ("a record whose " <> key <> " is " <> refinementOf value)
                                                    (Q.Step typePos (Q.Key key) keyType : below)
                       Broken err -> pure err

    -- A value outside the type it is checked against, at the value, with the trail below it
    failRefinement :: R.Type -> Text -> [Q.Step] -> EvalM L.Expr
    failRefinement required actual below =
        pure (refinementFailure pos ((failureOf role required typePos evaluatedVal actual Nothing) { Q.trail = below }) "")


-- A refinement defined in Ouro, titled: the value has it if it is in the refinement's domain,
-- the type its subject is declared with, and the refinement, run on it, answers true. The domain
-- is checked first, here, so that a value outside it is a value outside the refinement, the
-- refinement's first argument failing, which a question answers with false; only then is the
-- refinement run. A false answer is the value outside it, with the refinement's doc as the rule.
-- Any error running it stays the error; a failed requirement inside its body comes out with the
-- body as a step of its trail, so it is never taken for the value outside the refinement.
checkDefined :: Evaluator -> Q.Role -> SourcePos -> SourcePos -> Text -> L.Expr -> EvalM L.Expr
checkDefined ev role typePos pos name v = domainOf ev name >>= \case
    Just (domainPos, domain) -> checkRefinement ev (Q.ArgumentOf (Q.Defined name) 1) domain domainPos pos v >>= \case
                                    err@(EvalError _) -> pure err
                                    _inDomain         -> run
    Nothing                  -> run

    where
    run = runDefined ev typePos name v <&> \case
        Right (True, _)    -> v
        Right (False, doc) -> refinementFailure pos (failureOf role refinement typePos v given doc) (rule doc)
        Left err           -> throughStep (Q.Step typePos (Q.Body name) refinement) err

    refinement = R.Named (R.RDefined name)
    given      = aValue v

    -- The refinement's doc, stated as the rule it asserts
    rule = \case
        Just text -> name <> ": " <> text
        Nothing   -> name <> " has no doc, so the rule it asserts cannot be shown. "
                  <> "Perhaps write one: (define/refinement " <> name <> " (<r ⊢ record>) (doc \"...\") ...)."


-- A value outside a requirement, at the value: the failure as data, the text the printer shows
-- read off it, and how to put it right
refinementFailure :: SourcePos -> Q.Failure -> Text -> L.Expr
refinementFailure pos failure blurb =
    requirementFailed failure
    & (case T.null blurb of
           True  -> id
           False -> withBlurb blurb)
    & OuroError pos
    & EvalError


-- The failure of a requirement where it is checked: the role it was required in, the type,
-- where it is written, the value, its sketch and the refinement's doc. The trail is filled in as
-- the failure comes back out of the checks around it.
failureOf :: Q.Role -> R.Type -> SourcePos -> L.Expr -> Text -> Maybe Text -> Q.Failure
failureOf role required at value given doc = Q.Failure
    { Q.required   = required
    , Q.requiredAt = at
    , Q.given      = given
    , Q.givenKind  = kindOf value
    , Q.role       = role
    , Q.trail      = []
    , Q.rule       = doc
    }


-- What the check of one part of a type answered: in it; outside it, with the trail below the
-- part; or broken, the check itself an error, which stays that error
data Answer
    = In
    | Out [Q.Step]
    | Broken L.Expr


-- The requirement's failure an error carries, under any blurbs written on it; Nothing for any
-- other error
failureIn :: ErrorContext -> Maybe Q.Failure
failureIn = \case
    Typing (RefinementFailed _ _ failure) -> Just failure
    AnnotatedContext _ ctx                -> failureIn ctx
    _otherError                           -> Nothing


-- An error with a step put in front of its failure's trail, the boundary it came out through;
-- any other error unchanged
throughStep :: Q.Step -> L.Expr -> L.Expr
throughStep step = \case
    EvalError (OuroError at ctx) -> EvalError (OuroError at (onFailure ctx))
    other                        -> other

    where
    onFailure = \case
        Typing (RefinementFailed expected actual failure)
            -> Typing (RefinementFailed expected actual (failure { Q.trail = step : Q.trail failure }))

        AnnotatedContext blurb ctx
            -> AnnotatedContext blurb (onFailure ctx)

        other
            -> other


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

    -- A duration is a magnitude: xsd:duration's leading - is read, but is no duration of Ouro's
    (R.RDuration, Primitive (I.String rawText))
        -> case parseDuration rawText of
               Nothing                    -> Just ("a string that is not a duration", durationBlurb)
               Just d | negative d        -> Just ("a negative duration", durationBlurb)
                      | otherwise         -> Nothing

    (R.RDuration, _)
        -> Just (refinementOf value, durationBlurb)

    (kind, _)
        | hasKind kind value -> Nothing
        | otherwise          -> Just (refinementOf value, T.intercalate "\n\n" (filter (not . T.null) [fromMaybe "" (refinementTree kind value), T.strip (typeMismatchHint value)]))


uriBlurb, dateBlurb, durationBlurb :: Text
uriBlurb      = "A uri is a string with a scheme, such as \"https://linked.art/example/1\"."
dateBlurb     = "A date is a string in ISO-8601 form, YYYY-MM-DDTHH:mm:ssZ, such as \"1642-01-01T00:00:00Z\"."
durationBlurb = "A duration is a string in xsd:duration form, PnYnMnDTnHnMnS, such as \"P19D\" or \"PT3H\", "
             <> "a magnitude: the direction is the operation's, shift/forward or shift/backward. "
             <> "(days 19) writes one."


-- Whether an error says the value is outside a type, an answer of no, rather than that a check
-- broke: a requirement's failure decided on the value, its trail never inside a refinement's
-- body. A failure inside a body is the refinement broken, and a built-in's operand failing
-- inside one is too. Under any blurbs written on it.
isOutside :: ErrorContext -> Bool
isOutside = \case
    Typing (RefinementFailed _ _ failure) -> not (any (inBody . Q.stepPart) (Q.trail failure))
    AnnotatedContext _ ctx                -> isOutside ctx
    _otherError                           -> False

    where
    inBody = \case
        Q.Body _   -> True
        _otherPart -> False


-- Whether an error is a value lacking a refinement, a requirement's or a built-in operand's,
-- under any blurbs written on it
isRefinementMismatch :: ErrorContext -> Bool
isRefinementMismatch = \case
    Typing RefinementMismatch {} -> True
    Typing RefinementFailed {}   -> True
    AnnotatedContext _ ctx     -> isRefinementMismatch ctx
    _otherError                -> False
