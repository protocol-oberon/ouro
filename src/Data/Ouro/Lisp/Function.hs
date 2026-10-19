module Data.Ouro.Lisp.Function
( bindingWithoutValue
, checkFunctionName
, checkParams
, duplicateFunction
, functionBody
, graphBody
, isFunctionKeyword
, keyInLet
, misplacedBinding
, patternParam
, removedAttrForm
, removedContextForm
, removedDefine
, removedLocalDefn
, renamedDefun
, strayHole
, typedArgument
, typedResult
) where

import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import           Data.Ouro.Error.Diagnostics (invalidFunctionName, lexicalError,
                                              withBlurb)
import           Data.Ouro.Error.Types       (OuroError (..))
import qualified Data.Ouro.Lisp.Surface      as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Megaparsec             (SourcePos)


-- Signatures.
--
-- A function's types are written as tags in its header: one between the name and the
-- parameters for the result, and one before a parameter for that parameter:
--
--   (defn node #rec (#uri id type) (:id id :type type))
--   (fn #num (n) (* n 2))
--
-- A signature's tag checks a value just as the same tag would anywhere else, and
-- converts to a URI or a Date in the same way. The result is the body under the return
-- tag, and each argument is put under its parameter's tag where the call is written, so
-- a mismatch is reported at the argument. Nothing is inferred: an untyped parameter or
-- result is not checked.
typedResult :: Maybe (SourcePos, S.ReaderTag) -> S.Expr -> S.Expr
typedResult = maybe id (uncurry S.Tagged)

typedArgument :: S.Param -> S.Expr -> S.Expr
typedArgument param arg = maybe arg (\(pos, tag) -> S.Tagged pos tag arg) (S.paramType param)


-- Destructuring parameters.
--
-- A parameter can be a pattern, written as a case pattern is. It takes one argument, which
-- is matched against the pattern when the function is called, and the pattern's holes are
-- bound around the body:
--
--   (fn ((?slug ?label)) (person slug label))
--   (defn label-of ((:_label ?l)) l)
--
-- The parameter itself is named by its position, '%arg1', '%arg2', ..., which cannot be
-- written in Ouro, so its argument is only reached through the holes.
patternParam :: Int -> Maybe (SourcePos, S.ReaderTag) -> S.Expr -> S.Param
patternParam index tag pattern = S.Param ("%arg" <> T.pack (show index)) tag (Just pattern)


-- checkParams.
--
-- A pattern parameter must bind something, so it is a Record pattern or holds a '?' hole.
-- Each name a function binds, as a parameter or as a hole in a parameter's pattern, is
-- bound once. Checked when a function is defined.
checkParams :: SourcePos -> [S.Param] -> Either OuroError ()
checkParams pos params = do
    traverse_ checkPattern [ pattern | Just pattern <- map S.paramPattern params ]
    case repeated (concatMap bound params) of
        Just name -> lexicalError ("The parameter '" <> name <> "' is bound more than once.")
                     & withBlurb "Each parameter, and each hole in a parameter's pattern, names a different value. Perhaps rename one of them."
                     & OuroError pos
                     & Left
        Nothing   -> Right ()

    where
    checkPattern pattern
        | S.isPatternForm pattern
            = Right ()
        | otherwise
            = lexicalError "A parameter in parentheses is a pattern, but this one binds nothing."
              & withBlurb "A pattern names the parts of its argument with holes: (fn ((?a ?b)) ...) for an Array, (fn ((:key ?k)) ...) for a Record."
              & OuroError (S.exprPos pattern)
              & Left

    bound param = maybe [S.paramName param] holeNames (S.paramPattern param)

    -- The names a pattern's holes bind: '?' binds nothing, '?name' and '?..name' bind 'name'
    holeNames = \case
        S.Hole _ hole      -> [ name | let name = T.dropWhile (`elem` ['?', '.']) hole, not (T.null name) ]
        S.Form _ elems     -> concatMap holeNames elems
        S.Tagged _ _ inner -> holeNames inner
        _leaf              -> []

    repeated = go []
        where
        go seen = \case
            name : rest | name `elem` seen -> Just name
                        | otherwise        -> go (name : seen) rest
            []                             -> Nothing


-- functionBody.
--
-- A function's body is exactly one expression, written as it would be anywhere else;
-- its value is the function's result. Private values are bound around it with 'let':
--
--   (defn getty () "http://vocab.getty.edu/aat/")              ; a literal
--   (defn person (who) (let ($t "Person")                       ; a record
--                        (:type t :_label who)))
--
-- Checked when a function is defined, so a malformed function is reported even if it
-- is never called.
functionBody :: Text -> SourcePos -> [S.Expr] -> Either OuroError S.Expr
functionBody name pos body
    -- A ':key value' (or ':context') at the root of the body is a spliced record
    | splicedAttr : _ <- filter isSplicedAttr body
        = lexicalError ("The function '" <> name <> "' has a ':key value' attribute pair directly in its body.")
          & withBlurb ( "A function's body is one result expression, written as it would be anywhere else. "
                     <> "Perhaps wrap the record in parentheses: (defn " <> name <> " (args) (:key value ...))."
                      )
          & OuroError (S.exprPos splicedAttr)
          & Left

    | returnForm : _ <- filter isReturn body
        = lexicalError "'return' is no longer needed: a function's body is its result."
          & withBlurb "Perhaps write the value directly, e.g. (defn id (x) x) instead of (defn id (x) (return x))."
          & OuroError (S.exprPos returnForm)
          & Left

    | defineForm : _ <- filter (isHeadedBy "define") body
        = Left (removedDefine (S.exprPos defineForm))

    | defnForm : _ <- filter isLocalFunction body
        = Left (removedLocalDefn (S.exprPos defnForm))

    | otherwise
        = case body of
              [result]
                  -> Right result

              []
                  -> lexicalError ("The function '" <> name <> "' has no result expression.")
                     & withBlurb "A function's body is exactly one expression: its result."
                     & OuroError pos
                     & Left

              _result : extra : _
                  -> lexicalError ("The function '" <> name <> "' has more than one result expression.")
                     & withBlurb "A function returns one value. Perhaps wrap the values in parentheses to return an Array: (1 2) instead of 1 2."
                     & OuroError (S.exprPos extra)
                     & Left

    where
    isSplicedAttr = \case
                      S.Form _ (S.Symbol _ "%attr"   : _) -> True
                      S.Form _ (S.Symbol _ "context" : _) -> True
                      _notAField                          -> False

    isReturn = isHeadedBy "return"

    isLocalFunction = \case
                       S.Form _ (S.Symbol _ keyword : _) -> isFunctionKeyword keyword
                       _notAFunction                     -> False


-- graphBody.
--
-- A graph is one expression, usually a Record: (graph name (:key value ...)).
graphBody :: Text -> SourcePos -> [S.Expr] -> Either OuroError S.Expr
graphBody name pos = \case
    [S.Form _ (S.Symbol _ "%attr" : _)]
        -> spliced
    [result]
        -> Right result
    []
        -> lexicalError ("The graph '" <> name <> "' has no expression.")
           & withBlurb "A graph is one expression, usually a Record: (graph name (:key value ...))."
           & OuroError pos
           & Left
    _spliced
        -> spliced
    where
    spliced = lexicalError ("The graph '" <> name <> "' must be one expression.")
              & withBlurb ( "Perhaps wrap its attribute pairs in parentheses: (graph " <> name <> " (:key value ...)). "
                         <> "Private values are bound around it with let: (graph " <> name <> " (let ($name value) (:key value ...)))."
                          )
              & OuroError pos
              & Left


-- '(attr key value)' was removed: an attribute pair is written ':key value', and a key is
-- never computed, since JSON-LD's keys are vocabulary terms
removedAttrForm :: SourcePos -> OuroError
removedAttrForm pos =
    lexicalError "The (attr key value) form has been removed."
    & withBlurb ( "An attribute pair is written :key value, as in (:_label \"Proust\"), and insert takes one the same way: "
               <> "(insert 0 :_label \"Proust\" record). A key can no longer be computed from a value."
                )
    & OuroError pos


-- '(context (...))' was replaced by ':context value': a record's context is an ordinary
-- value, written as JSON-LD writes it
removedContextForm :: SourcePos -> OuroError
removedContextForm pos =
    lexicalError "The (context (...)) form has been replaced by ':context value'."
    & withBlurb ( "A record's context is written as JSON-LD writes it: a remote context, (:context \"https://...\" ...); "
               <> "a local context, (:context (:@vocab \"https://...\") ...); "
               <> "or several, in order, (:context (\"https://...\" (:term \"https://...\")) ...)."
                )
    & OuroError pos


-- 'define' was replaced by 'let', which binds private values around the one
-- expression that uses them
removedDefine :: SourcePos -> OuroError
removedDefine pos =
    lexicalError "'define' has been replaced by let."
    & withBlurb "Bind private values around the expression that uses them: (let ($name value ...) expr)."
    & OuroError pos


-- ':' marks a key in the output, so it does not belong in a let, whose bindings are
-- never output. A private binding is written '$name value'.
keyInLet :: Text -> SourcePos -> OuroError
keyInLet name pos =
    lexicalError ("':" <> name <> "' marks a key in the output, but a let's bindings are never output.")
    & withBlurb ( "A let binds private values with '$': (let ($" <> name <> " value ...) expr). "
               <> "To output the value, write ':" <> name <> " value' in a record."
                )
    & OuroError pos


-- '$' only defines a private binding, in a let. Written anywhere else it is usually a
-- reference with a sigil it does not need, or a binding outside its let.
misplacedBinding :: Text -> SourcePos -> OuroError
misplacedBinding name pos =
    lexicalError ("'$" <> name <> "' is a private binding, which can only be written in a let.")
    & withBlurb ( "To refer to a binding, write its name without '$': " <> name <> ". "
               <> "To output a value, write ':" <> name <> " value' in a record; "
               <> "to bind one, write (let ($" <> name <> " value) expr)."
                )
    & OuroError pos


-- A '$name' in a let with no value after it
bindingWithoutValue :: Text -> SourcePos -> OuroError
bindingWithoutValue name pos =
    lexicalError ("'$" <> name <> "' has no value.")
    & withBlurb ("A let binds each name to the value written after it: (let ($" <> name <> " value ...) expr).")
    & OuroError pos


-- A 'defn' only names a top-level function; a local function is a let-bound 'fn'
removedLocalDefn :: SourcePos -> OuroError
removedLocalDefn pos =
    lexicalError "A defn can only appear at the top level of a file."
    & withBlurb "Write a local function with fn, bound by let: (let ($name (fn (args) body)) expr)."
    & OuroError pos


-- A '?' hole is a placeholder among a call's arguments, or a value in a pattern (of a case
-- branch or a parameter). A rest hole only matches the rest of an Array or Record, so it
-- belongs in a pattern.
strayHole :: Text -> SourcePos -> OuroError
strayHole hole pos
    | T.isPrefixOf "?.." hole
        = lexicalError ("'" <> hole <> "' matches the rest of an Array or Record, so it can only be written in a pattern.")
          & withBlurb "Perhaps write it in a case branch, (case value ((?head ?..rest) rest) (otherwise ())), or a parameter, (fn ((?head ?..rest)) rest)."
          & OuroError pos
    | otherwise
        = lexicalError ("'" <> hole <> "' is a hole, which can only be written as an argument of a call or in a pattern.")
          & withBlurb "A call with holes is a function of them: (+ ? 1) is (fn (n) (+ n 1))."
          & OuroError pos


-- 'defun' was renamed 'defn'
renamedDefun :: SourcePos -> OuroError
renamedDefun pos =
    lexicalError "'defun' has been renamed defn."
    & withBlurb "Write a top-level function as (defn name (args) expr)."
    & OuroError pos


-- The keywords that define a named function: 'defn', and the old 'defun', which is
-- still recognised so that it can be reported
isFunctionKeyword :: Text -> Bool
isFunctionKeyword = (`elem` ["defn", "defun"])


-- Function names are plain symbols, in one namespace with values. A trailing '!'
-- is allowed but not required. A name cannot be a special form: special forms are
-- recognised before names are looked up, so such a function could never be called.
checkFunctionName :: Text -> SourcePos -> Either OuroError ()
checkFunctionName name pos
    | name `elem` specialForms
        = invalidFunctionName name
          & withBlurb ("'" <> name <> "' is a special form, so it cannot be used as a function name. Perhaps choose another name.")
          & OuroError pos
          & Left
    | otherwise
        = Right ()


-- A function can be defined once in each scope
duplicateFunction :: Text -> SourcePos -> OuroError
duplicateFunction name pos =
    lexicalError ("The function '" <> name <> "' is defined more than once in the same scope.")
    & withBlurb "Perhaps rename one of the definitions, or remove the one you no longer need."
    & OuroError pos


-- Forms the evaluator recognises by their head symbol, before any name lookup
specialForms :: [Text]
specialForms = [ "->", "all", "attr", "case", "context", "define", "defn", "defun", "fn", "get"
               , "graph", "inlay", "insert", "let", "list", "map", "nth", "replace-at", "some"
               , "thru", "with" ]


isHeadedBy :: Text -> S.Expr -> Bool
isHeadedBy keyword = \case
                      S.Form _ (S.Symbol _ head' : _) -> head' == keyword
                      _otherExpr                      -> False
