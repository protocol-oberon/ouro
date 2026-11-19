{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Parser where

import           Control.Monad               (foldM, foldM_, when)
import           Control.Monad.State.Strict  (MonadTrans (lift),
                                              StateT (runStateT), gets,
                                              modify')
import           Data.Foldable               (find, traverse_)
import qualified Data.Function               as F
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Ouro.Error.Diagnostics (lexicalError, missingAttributeValue,
                                              unbalancedDelimiter, withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import           Data.Ouro.Lisp.Function     (checkFunctionName, checkParams,
                                              duplicateFunction, functionBody,
                                              functionDoc, graphBody,
                                              isFunctionKeyword, patternParam,
                                              refinementParams, refinementResult,
                                              removedLocalDefn)
import           Data.Ouro.Lisp.Module.Types (Declaration (..),
                                              FunctionKind (..),
                                              HigherExpression (..),
                                              Import (..), Module (..),
                                              SomeFunction (..),
                                              exports, functionRegistry,
                                              graphRegistry, imports,
                                              qualifierOf)
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Lisp.Tokens       as Tkn
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro.Platform         (at, (%~), (&), (.~), (?~), (^.))
import           Text.Megaparsec             (SourcePos)
import qualified Text.Megaparsec.Pos         as M


-- The parser's state: the tokens still to read, and the innermost delimiter still open, with
-- the token that closes it. Running out of tokens is reported at that delimiter, since a file
-- that ends early always leaves one open, and the token stream has no position past its end
data ParseState = ParseState
    { remaining :: [Tkn.Token]
    , opened    :: (SourcePos, Text)
    }

type Parser a = StateT ParseState (Either OuroError) a

remainingTokens :: Parser [Tkn.Token]
remainingTokens = gets remaining

consume :: [Tkn.Token] -> Parser ()
consume ts = modify' (\s -> s { remaining = ts })

-- Parses inside a delimiter opened at a position, which the token given closes
within :: SourcePos -> Text -> Parser a -> Parser a
within pos closer p = do
    outer  <- gets opened
    modify' (\s -> s { opened = (pos, closer) })
    result <- p
    modify' (\s -> s { opened = outer })
    pure result

-- The file ended inside the innermost delimiter still open
endOfFile :: Parser a
endOfFile = do
    (pos, closer) <- gets opened
    lift (Left (OuroError pos (unbalancedDelimiter ("'" <> closer <> "' to close this") "the end of the file")))

-- Wraper for type indexed higher expressions
data ParsedDecl
    = PFunction SomeFunction
    | PGraph    (HigherExpression 'GraphExpr)
    | PImport   Import
    | PExport   SourcePos [(Text, SourcePos)]


-- Top-level entry point
parseModule :: [Tkn.Token] -> Either OuroError Module
parseModule tokens = do
    -- Nothing is read before the first form opens, so its first token stands in until then
    let start = case tokens of
                    t : _ -> Tkn.pos t
                    []    -> M.initialPos ""
    (decls, _) <- runStateT (collectHigherExpressions []) (ParseState tokens (start, ")"))
    ouroModule <- buildModuleRegistry decls
    checkExports ouroModule
    pure ouroModule


-- Collection Pass (Building the structured env)
buildModuleRegistry :: [ParsedDecl] -> Either OuroError Module
buildModuleRegistry = foldM insertDecl emptyModule
    where
    emptyModule = Module Map.empty Map.empty [] [] Set.empty

    insertDecl :: Module -> ParsedDecl -> Either OuroError Module
    insertDecl m = \case
                    (PFunction t@(SomeFunction (Function _ pos name _ _ _ _)))
                        -> do
                           checkFunctionName name pos
                           case Map.member name (m ^. functionRegistry) of
                               True  -> Left (duplicateFunction name pos)
                               False -> Right $ m & functionRegistry . at name ?~ t
                    (PGraph    g@(Graph    _ name body)) -> do
                                                            validateGraphBody body
                                                            Right $ m & graphRegistry    . at name ?~ g
                    (PImport imp)
                        -> do
                           comesFirst m (importPos imp)
                           reservedQualifier imp
                           case filter ((== qualifierOf imp) . qualifierOf) (m ^. imports) of
                               earlier : _ -> Left (qualifierClash earlier imp)
                               []          -> Right $ m & imports %~ (<> [imp])
                    (PExport pos names)
                        -> do
                           comesFirst m pos
                           case m ^. exports of
                               []         -> Right $ m & exports .~ names
                               _secondOne -> lexicalError "A module has one export, naming every function it exports."
                                             F.& withBlurb "Perhaps join them into one: (export f g)."
                                             F.& OuroError pos
                                             F.& Left

    -- Imports and exports are the first thing in a file: they say what it depends on and offers
    comesFirst m pos =
        when (not (Map.null (m ^. functionRegistry) && Map.null (m ^. graphRegistry))) $
            lexicalError "Imports and exports come first in a file, before any definition or graph."
            F.& withBlurb "Perhaps move it to the top of the file, so the file's dependencies and exports are the first thing read."
            F.& OuroError pos
            F.& Left

    -- define/procedure, define/refinement and define/constant are keywords, and defn/const and
    -- defn/property retired ones, so the functions of an import qualified define or defn would
    -- read as them. 'as define' and 'as defn' cannot be written (each on its own is an error); a
    -- path ending in either can.
    reservedQualifier imp =
        case lookup (qualifierOf imp) reservedQualifiers of
            Just keywords
                -> lexicalError ("The import " <> importPath imp <> " would be qualified '" <> qualifierOf imp <> "', which begins the keywords " <> keywords <> ".")
                   F.& withBlurb ("Perhaps name it with as: (import " <> importPath imp <> " as another-name).")
                   F.& OuroError (importPos imp)
                   F.& Left
            Nothing
                -> Right ()

    reservedQualifiers = [ ("define", "define/procedure, define/refinement and define/constant")
                         , ("defn",   "defn/const and defn/property, which define/constant and define/refinement replace") ]

    qualifierClash earlier imp =
        lexicalError ("Two imports are both qualified '" <> qualifierOf imp <> "': " <> importPath earlier <> " and " <> importPath imp <> ".")
        F.& withBlurb ( "Each import's functions are written qualifier/name, so two imports need two qualifiers. "
                     <> "Perhaps name one with as: (import " <> importPath imp <> " as another-name)."
                      )
        F.& OuroError (importPos imp)

    -- Semantic validation
    validateGraphBody :: S.Expr -> Either OuroError ()
    validateGraphBody =
        \case
         S.Form pos (S.Symbol _ keyword : _)
             | isFunctionKeyword keyword        -> Left (removedLocalDefn pos)
         S.Form _   exprs                       -> traverse_ validateGraphBody exprs
         _safeLeafs                             -> Right ()


-- Every exported name is a function of this module, named once
checkExports :: Module -> Either OuroError ()
checkExports m = foldM_ check Set.empty (m ^. exports)
    where
    defined = Map.keys (m ^. functionRegistry)

    check seen (name, pos)
        | name `Set.member` seen
            = lexicalError ("The function '" <> name <> "' is exported twice.")
              F.& withBlurb "Perhaps remove one of them from the export."
              F.& OuroError pos
              F.& Left
        | not (name `elem` defined)
            = lexicalError ("The module exports '" <> name <> "', but defines no function by that name.")
              F.& withBlurb (exportBlurb name)
              F.& OuroError pos
              F.& Left
        | otherwise
            = Right (Set.insert name seen)

    exportBlurb name =
        case rankBySimilarity name defined of
            (closest, score) : _ | score <= 3 -> "Perhaps you meant '" <> closest <> "'."
            _noneClose                         -> "A module exports its own functions only, defined in the same file."


collectHigherExpressions :: [ParsedDecl] -> Parser [ParsedDecl]
collectHigherExpressions acc = do
    tokens <- remainingTokens
    case tokens of
        []         -> pure (reverse acc) -- EOF reached safely
        _hasTokens -> do
                      decl <- pHigherExpression
                      collectHigherExpressions (decl : acc)


pHigherExpression :: Parser ParsedDecl
pHigherExpression = do
    startTkn <- popToken
    case Tkn.tokenType startTkn of
        Tkn.OpenParen -> within (Tkn.pos startTkn) ")" $ do
                         kwTkn <- popToken
                         case Tkn.tokenType kwTkn of
                             Tkn.DefineProcedure  -> pDefn ProcedureKind  pArgs     (Tkn.pos startTkn)
                             Tkn.DefineConstant   -> pDefn ProcedureKind  (pure []) (Tkn.pos startTkn)
                             Tkn.DefineRefinement -> pDefn RefinementKind pArgs     (Tkn.pos startTkn)
                             Tkn.Graph            -> pGraph (Tkn.pos startTkn)
                             Tkn.Import           -> pImport (Tkn.pos startTkn)
                             Tkn.Export           -> pExport (Tkn.pos startTkn)
                             other                -> lexicalError ("'" <> Tkn.written other <> "' cannot begin a form at the top level of a file")
                                                     F.& withBlurb topLevelForms
                                                     F.& OuroError (Tkn.pos kwTkn)
                                                     F.& Left
                                                    F.& lift
        other -> lexicalError ("A top-level form begins with '(', not '" <> Tkn.written other <> "'")
                 F.& withBlurb topLevelForms
                 F.& OuroError (Tkn.pos startTkn)
                 F.& Left
                 F.& lift
    where
    topLevelForms = "A file's top level holds (define/procedure ...), (define/constant ...), (graph ...), (import ...) and (export ...) forms; anything else is written inside one of them."


-- (define/procedure name (params) body), or (define/constant name body): a function of no
-- parameters, with none written. Either may have a return type after its name.
-- (define/refinement name (params) body) is a refinement: a function of the refinement kind.
pDefn :: FunctionKind k -> Parser [S.Param] -> SourcePos -> Parser ParsedDecl
pDefn kind params pos = do
    name    <- expectSymbol "Expected a function name"
    written <- pReturnType
    args    <- params
    body    <- collectUntil Tkn.CloseParen

    -- A pattern parameter binds something, and each name is bound once
    lift (checkParams pos args)

    -- A refinement's result is #boolean, and its one parameter has a contract
    retType <- case kind of
                   ProcedureKind  -> pure written
                   RefinementKind -> do
                                     lift (refinementParams name pos args)
                                     lift (refinementResult pos written)

    -- A (doc "...") among the body's direct elements is the function's documentation
    (doc, results) <- lift (functionDoc name body)

    -- A body is exactly one result expression, checked against the return type when the
    -- function is called
    result <- lift (functionBody name pos results)

    pure $ PFunction (SomeFunction (Function kind pos name doc args retType (S.Form pos [result])))


-- A function's return type, between its name and its parameters: ⊢ and its refinement,
-- (define/procedure f ⊢ string (x) ...), or a type tag, (define/procedure f #string (x) ...)
pReturnType :: Parser (Maybe (SourcePos, S.Type))
pReturnType = do
    stream <- remainingTokens
    case stream of
        t : ts | Tkn.Symbol "assert" <- Tkn.tokenType t
            -> do
               consume ts
               written <- pExpr
               Just <$> lift (requirementOf written)
        t : ts | Just tag <- typeTag (Tkn.tokenType t)
            -> consume ts >> pure (Just (Tkn.pos t, S.Named tag))
        _noReturnType
            -> pure Nothing


-- A requirement as a signature writes it, after ⊢: a refinement's name, (⊢ string) or
-- <r ⊢ titled>, or ∪ ∩ ∖ of types, <r ⊢ (∪ painting print)>. Every signature's requirement
-- is read here, so that a requirement written as more, such as one applied to an earlier
-- parameter, (has-part-of-type kind), is read in this one place when there are such.
requirementOf :: S.Expr -> Either OuroError (SourcePos, S.Type)
requirementOf written = case S.readType written of
    Right ty
        -> Right (S.exprPos written, ty)
    Left other
        -> lexicalError "A type is a refinement's name, or ∪ ∩ ∖ of two types: <x ⊢ string>, <x ⊢ (∪ string number)>."
           F.& withBlurb "A refinement of the module is named the same way: <r ⊢ titled>."
           F.& OuroError (S.exprPos other)
           F.& Left


pGraph :: SourcePos -> Parser ParsedDecl
pGraph pos = do
    name <- expectSymbol "Expected graph name"
    body <- collectUntil Tkn.CloseParen

    -- A graph is one expression, usually a Record
    expr <- lift (graphBody name pos body)

    pure $ PGraph (Graph pos name expr)


-- (import path) or (import path as alias). A path is names joined by '/', and an alias is
-- one name, since it qualifies the module's functions: alias/name.
pImport :: SourcePos -> Parser ParsedDecl
pImport pos = do
    path   <- expectSymbol importForm
    lift (checkPath path)
    stream <- remainingTokens
    alias  <- case stream of
                  Tkn.Token _ (Tkn.Symbol "as") : Tkn.Token aliasPos (Tkn.Symbol name) : rest
                      -> do
                         consume rest
                         lift (checkAlias aliasPos name)
                         pure (Just name)
                  _noAlias
                      -> pure Nothing
    closing <- popToken
    case Tkn.tokenType closing of
        Tkn.CloseParen -> pure (PImport (Import pos path alias))
        _somethingElse -> lexicalError importForm
                          F.& OuroError (Tkn.pos closing)
                          F.& Left
                          F.& lift

    where
    importForm = "An import is written (import path) or (import path as name)."

    checkPath path =
        when (any T.null (T.splitOn "/" path)) $
            lexicalError ("The module path '" <> path <> "' has an empty part.")
            F.& withBlurb "A module path is names joined by '/', as in linked-art/v1/people."
            F.& OuroError pos
            F.& Left

    checkAlias aliasPos name =
        when (T.any (== '/') name) $
            lexicalError ("The import's name '" <> name <> "' has a '/' in it.")
            F.& withBlurb "An import's name qualifies its functions, name/function, so it is one name: (import linked-art/v1/people as people)."
            F.& OuroError aliasPos
            F.& Left


-- (export f g ...): the functions this module offers its importers
pExport :: SourcePos -> Parser ParsedDecl
pExport pos = collectNames []
    where
    collectNames acc = do
        t <- popToken
        case Tkn.tokenType t of
            Tkn.CloseParen
                | null acc     -> lexicalError "An export names at least one function: (export f g)."
                                  F.& OuroError pos
                                  F.& Left
                                  F.& lift
                | otherwise    -> pure (PExport pos (reverse acc))
            Tkn.Symbol name    -> collectNames ((name, Tkn.pos t) : acc)
            _notAName          -> lexicalError "An export names the module's functions: (export f g)."
                                  F.& OuroError (Tkn.pos t)
                                  F.& Left
                                  F.& lift


popToken :: Parser Tkn.Token
popToken = do
    stream <- remainingTokens
    case stream of
        []     -> endOfFile
        (t:ts) -> consume ts >> pure t


expectSymbol :: Text -> Parser Text
expectSymbol err = do
    t <- popToken
    case Tkn.tokenType t of
        Tkn.Symbol name -> pure name
        _notASymbol     -> lexicalError err
                           F.& OuroError (Tkn.pos t)
                           F.& Left
                           F.& lift


expectCloseParen :: Parser ()
expectCloseParen = do
    t <- popToken
    case Tkn.tokenType t of
        Tkn.CloseParen -> pure ()
        _notAParen     -> unbalancedDelimiter "Expected ')'" "Found something else"
                          F.& OuroError (Tkn.pos t)
                          F.& Left
                          F.& lift


-- A parameter list: names or patterns, each optionally typed by a binder or by the old tag
-- before it, (<id ⊢ uri> type), (#uri id type) or ((?slug ?label))
pArgs :: Parser [S.Param]
pArgs = do
    t <- popToken
    case Tkn.tokenType t of
        Tkn.OpenParen -> within (Tkn.pos t) ")" (collectArgs [])
        _other        -> lexicalError "Expected argument list starting with '('"
                         F.& OuroError (Tkn.pos t)
                         F.& Left
                         F.& lift

    where
    collectArgs acc = do
        t <- popToken
        case Tkn.tokenType t of
            Tkn.CloseParen      -> pure $ reverse acc
            Tkn.Symbol     name -> collectArgs (S.Param name Nothing Nothing : acc)
            Tkn.OpenParen       -> do
                                   pattern <- parseFormContainer (Tkn.pos t)
                                   collectArgs (patternParam (length acc + 1) Nothing pattern : acc)
            Tkn.OpenAngle       -> do
                                   binder <- parseInfix (Tkn.pos t)
                                   param  <- lift (binderParam (length acc + 1) (Tkn.pos t) binder)
                                   collectArgs (param : acc)
            tkn | Just tag <- typeTag tkn
                                -> do
                                   stream <- remainingTokens
                                   case stream of
                                       next : rest | Tkn.tokenType next == Tkn.OpenParen
                                           -> do
                                              consume rest
                                              pattern <- parseFormContainer (Tkn.pos next)
                                              collectArgs (patternParam (length acc + 1) (Just (Tkn.pos t, S.Named tag)) pattern : acc)
                                       _aName
                                           -> do
                                              name <- expectSymbol "A parameter's type is written before its name, as in (#number n)"
                                              collectArgs (S.Param name (Just (Tkn.pos t, S.Named tag)) Nothing : acc)
            _notAParameter      -> lexicalError "A parameter is a name, or a pattern in parentheses, each optionally typed: (#number n) or ((?a ?b))"
                                   F.& OuroError (Tkn.pos t)
                                   F.& Left
                                   F.& lift


-- A parameter written as a binder, <n ⊢ number>, read as an infix expression: a name or a
-- pattern, then ⊢ and one requirement. A binder asserts, so another operator, <x ∈ p>, is
-- an error, as is a chain of requirements, which wait for a requirement's intersection.
binderParam :: Int -> SourcePos -> S.Expr -> Either OuroError S.Param
binderParam index pos = \case
    S.Form _ [S.Symbol _ "%infix", target, S.Symbol _ "assert", written]
        -> do
           requirement <- requirementOf written
           case target of
               S.Symbol _ name      -> Right (S.Param name (Just requirement) Nothing)
               pattern@(S.Form _ _) -> Right (patternParam index (Just requirement) pattern)
               _notAParameter       -> notABinder
    _otherInfix
        -> notABinder

    where
    notABinder = lexicalError "A parameter's binder is a name or a pattern, then ⊢ and its refinement: <n ⊢ number>, <(?a ?b) ⊢ array>."
                 F.& withBlurb "A binder asserts one refinement, so ∈, or a chain of them, is not a binder."
                 F.& OuroError pos
                 F.& Left


-- The tags that name a type, in an expression or a signature. '#record-empty' and
-- '#array-empty' are values, not types.
typeTag :: Tkn.TokenType -> Maybe S.Refinement
typeTag = \case
    Tkn.TagUri      -> Just S.RUri
    Tkn.TagDate     -> Just S.RDate
    Tkn.TagString   -> Just S.RString
    Tkn.TagNumber   -> Just S.RNumber
    Tkn.TagBoolean  -> Just S.RBoolean
    Tkn.TagRecord   -> Just S.RRecord
    Tkn.TagArray    -> Just S.RArray
    Tkn.TagFunction -> Just S.RFunction
    Tkn.TagRefinement name -> Just (S.RDefined name)
    _notAType       -> Nothing

-- Recursive Expr Router
pExpr :: Parser S.Expr
pExpr = do
        tokens <- remainingTokens
        case tokens of
            []     -> endOfFile
            (t:ts) -> let capture n = consume ts >> pure n
                      in case Tkn.tokenType t of
                             -- Open boundaries push processing down into structural lookahead groups
                             -- Note: We intentionally pop 'ts' here to advance past the open delimiter
                             Tkn.OpenParen      -> consume ts >> parseFormContainer (Tkn.pos t)

                             -- Primitive Leaf Node capture (+ source location)
                             Tkn.Let            -> capture (S.Symbol (Tkn.pos t) "let")
                             Tkn.DefineProcedure -> capture (S.Symbol (Tkn.pos t) "define/procedure")
                             Tkn.DefineConstant  -> capture (S.Symbol (Tkn.pos t) "define/constant")
                             Tkn.DefineRefinement -> capture (S.Symbol (Tkn.pos t) "define/refinement")

                             -- Parameterized Identifiers
                             Tkn.Symbol         txt -> capture (S.Symbol (Tkn.pos t) txt)

                             -- Attributes (Reader Macro -> `(%attr "key" expr)`)
                             Tkn.Attr           txt -> consume ts >> parseAttrNode (Tkn.pos t) txt
                             Tkn.Binding        txt -> capture (binderNode (Tkn.pos t) txt)

                             -- Core Data Literals
                             Tkn.String txt     -> capture (S.Literal (Tkn.pos t) (S.Str txt))
                             Tkn.Number val     -> capture (S.Literal (Tkn.pos t) (S.Num val))
                             Tkn.Boolean b      -> capture (S.Literal (Tkn.pos t) (S.Bool b))

                             -- Reader Macros & Type Assertions
                             -- Note: We intentionally pop 'ts' here to advance past the macro tag
                             tkn | Just tag <- typeTag tkn
                                                -> consume ts >> parseTaggedNode (Tkn.pos t) (S.Named tag)

                             -- Empty Collections instantiate as direct fallback values
                             Tkn.TagRecordEmpty -> capture (S.Literal (Tkn.pos t) S.EmptyRec)
                             Tkn.TagArrayEmpty  -> capture (S.Literal (Tkn.pos t) S.EmptyArr)

                             -- Hole Type
                             Tkn.Hole txt       -> capture (S.Hole (Tkn.pos t) txt)

                             -- A range's '..' is read by the form around it; anywhere else it has no meaning
                             Tkn.DotDot         -> lift (Left (misplacedRange (Tkn.pos t)))

                             -- An infix expression, <a lt b>, read to its '>'; a '>' alone closes
                             -- none, and is most likely a comparison's old name
                             Tkn.OpenAngle      -> consume ts >> parseInfix (Tkn.pos t)
                             Tkn.CloseAngle     -> lexicalError "This '>' closes no '<'."
                                                   & withBlurb "A comparison is a word: (gt a b), and (geq a b) for at least."
                                                   & OuroError (Tkn.pos t)
                                                   & Left
                                                   & lift

                             -- Unbalanced Boundaries are immediate semantic loop violations
                             Tkn.CloseParen -> let context = Syntax UnbalancedDelimiter
                                                                 { expectedDelim = "Opening Form Boundary '('"
                                                                 , actualDelim   = "Orphaned Closing Parenthesis ')'"
                                                                 }
                                               in lift $ Left (OuroError (Tkn.pos t) context)

                             _invalidToken -> lexicalError ("Invalid token: " <> (T.pack $ show (Tkn.tokenType t)) <> " found in expression")
                                              & OuroError (Tkn.pos t)
                                              & Left
                                              & lift


-- Lookahead Container Accumulators
parseFormContainer :: SourcePos -> Parser S.Expr
parseFormContainer startPos = do
                              terms <- within startPos ")" (collectUntil Tkn.CloseParen)
                              case (terms, [ pos | S.Symbol pos "%.." <- terms ]) of
                                  -- (start..end) and (first second .. end), Haskell's ranges, are
                                  -- calls no one can write
                                  ([from, S.Symbol pos "%..", to], [_])
                                      | not (any anItem [from, to])
                                      -> pure $ S.Form startPos [S.Symbol pos "%range", from, to]
                                  ([from, next, S.Symbol pos "%..", to], [_])
                                      | not (any anItem [from, next, to])
                                      -> pure $ S.Form startPos [S.Symbol pos "%range", from, next, to]
                                  (_, pos : _)
                                      -> lift (Left (misplacedRange pos))
                                  (_, [])
                                      -> pure $ S.Form startPos terms


-- An infix expression, <a op b>, as SRFI 105 reads {a op b}, written (%infix a op b) for
-- 'Canon.Infix' to make prefix. Its shape is checked here, so that pass cannot fail: an
-- operator between each two operands, every one a name and the same name, since there is no
-- precedence, and nesting is written, <<a + b> * c>. A name chains only where a left fold
-- means something, its result the kind of its first operand; a relation's chain,
-- <a lt b lt c>, is written as its pairs.
parseInfix :: SourcePos -> Parser S.Expr
parseInfix startPos = do
    terms <- within startPos ">" (collectUntil Tkn.CloseAngle)
    lift (infixForm startPos terms)

infixForm :: SourcePos -> [S.Expr] -> Either OuroError S.Expr
infixForm startPos terms =
    case (length terms >= 3 && odd (length terms), traverse operatorName operators) of
        (False, _)
            -> shapeError

        (True,  Nothing)
            -> notAName

        (True,  Just [])
            -> shapeError
        (True,  Just (name : more))
            -> case (filter (/= name) more, more) of
                   (other : _, _) -> mixed name other
                   ([], [])       -> written
                   ([], _chain)   -> case name `elem` chaining of
                                         True  -> written
                                         False -> doesNotChain name

    where
    written = Right (S.Form startPos (S.Symbol startPos "%infix" : terms))

    -- Every second element, the operators between the operands
    operators = [ term | (index, term) <- zip [0 :: Int ..] terms, odd index ]

    operatorName = \case
        S.Symbol _ name -> Just name
        _notAName       -> Nothing

    -- The operators a chain folds through: each gives back the kind of its first operand
    chaining  = ["+", "-", "*", "/", "and", "or", "union", "intersect", "setminus", "assert"]
    relations = ["lt", "leq", "gt", "geq", "eq", "neq", "in"]

    -- An error at the first operator that meets the condition, or at the '<'
    failAt found message blurb =
        Left (OuroError (maybe startPos S.exprPos (find found operators)) (withBlurb blurb (lexicalError message)))

    shapeError = failAt (const False)
                        "An infix expression has an operator between each two operands: <a op b>."
                        "A single value needs no brackets, and an operation of one operand is a call: (not b)."
    notAName   = failAt (isNothing . operatorName)
                        "An infix operator is a name: <a op b>."
                        "Write the operation as a call, or bind it to a name first."
    mixed name other
               = failAt ((== Just other) . operatorName)
                        ("'" <> name <> "' and '" <> other <> "' are mixed in one infix expression.")
                        ("There is no precedence: nest one, <<a " <> name <> " b> " <> other <> " c>.")
    doesNotChain name
               = failAt (const False)
                        ("'" <> name <> "' does not chain.")
                        (case name `elem` relations of
                             True  -> "Write each pair: (all (<a " <> name <> " b> <b " <> name <> " c>))."
                             False -> "Nest it: <<a " <> name <> " b> " <> name <> " c>.")


-- An attribute pair or a binding, which the parser writes as a form headed by a name that
-- cannot be written, (%attr ...), (%binder ...): an item of a record or a let, never a value
anItem :: S.Expr -> Bool
anItem = \case
    S.Form _ (S.Symbol _ head' : _) -> "%" `T.isPrefixOf` head'
    _notAnItem                      -> False


-- A '..' that is not between the ends of a range, alone in its parentheses
misplacedRange :: SourcePos -> OuroError
misplacedRange pos =
    lexicalError "A range is written (start..end), or (first second .. end) to count in steps, with nothing else in its parentheses."
    F.& withBlurb "(1..5) is (1 2 3 4 5), both ends included, as in Haskell; (2 4 .. 10) is (2 4 6 8 10), and (5 4 .. 1) counts down. The ends are expressions: (1..(len xs))."
    F.& OuroError pos


-- Reader Tag Helper: Verifies that the evaluated expression is a raw string payload
parseTaggedNode :: SourcePos -> S.Type -> Parser S.Expr
parseTaggedNode tagPos tagType = do
                                 stream <- remainingTokens
                                 case stream of
                                     -- A refinement on its own, at the end of a form: (satisfies x #string),
                                     -- or a case head's (satisfies #string). Its subject is a hole that
                                     -- cannot be written in Ouro: satisfies takes its value before it,
                                     -- and anywhere else it is reported
                                     t : _ | Tkn.CloseParen <- Tkn.tokenType t
                                         -> pure $ S.Asserted tagPos tagType (S.Hole tagPos "%subject")

                                     -- Otherwise the refinement applies to the expression after it
                                     _anExpression
                                         -> S.Asserted tagPos tagType <$> pExpr


-- Reader Macro Helper: Desugars attributes into `(%attr "name" expr)`. '%' cannot be
-- lexed, so the form cannot be written: an attribute pair is always written :key value. A key
-- followed by another key, or by the end of its form, has no value, and is an error at the key:
-- read as a value, the next key would take the pair into itself, and both would be lost.
parseAttrNode :: SourcePos -> Text -> Parser S.Expr
parseAttrNode pos attrName = do
                             stream <- remainingTokens
                             case map Tkn.tokenType (take 1 stream) of
                                 [Tkn.Attr next]    -> noValue ("the key ':" <> next <> "'")
                                 [Tkn.CloseParen]   -> noValue "the end of its form"
                                 _aValue            -> do
                                                       -- Eagerly consume the next expression to bind to the attribute
                                                       nextExpr <- pExpr
                                                       pure $ S.Form pos [ S.Symbol pos "%attr"
                                                                         , S.Literal pos (S.Str attrName)
                                                                         , nextExpr
                                                                         ]
    where
    noValue followedBy = lift (Left (OuroError pos (missingAttributeValue attrName followedBy)))


-- A private binding's name, '$name', read on its own as (%binder "name"). It never takes
-- the next expression: inside a let's binding list the desugarer pairs it with its value
-- ('Canon.pairBindings'), and anywhere else it is reported, so a '$name' written where a
-- reference was meant cannot swallow its neighbour. '%' cannot be written in Ouro, so the
-- head cannot be confused with a user's function.
binderNode :: SourcePos -> Text -> S.Expr
binderNode pos name = S.Form pos [S.Symbol pos "%binder", S.Literal pos (S.Str name)]


-- Parsing Stream State Helpers
-- Collects expressions sequentially until a targeted closing delimiter is reached.
collectUntil :: Tkn.TokenType -> Parser [S.Expr]
collectUntil targetDelim =
    do
    stream <- remainingTokens
    case stream of
        -- The token stream ran dry before finding the matching delimiter
        [] -> endOfFile

        (t:ts) | Tkn.tokenType t == targetDelim -> do
                    consume ts  -- Pop the matching closing token out of stream state
                    pure []
               -- A range's '..', in parentheses, which 'parseFormContainer' reads; '%' cannot
               -- be written, so the marker cannot be
               | Tkn.tokenType t == Tkn.DotDot, targetDelim == Tkn.CloseParen -> do
                    consume ts
                    rest <- collectUntil targetDelim
                    pure (S.Symbol (Tkn.pos t) "%.." : rest)
               | otherwise -> do
                    current <- pExpr
                    rest    <- collectUntil targetDelim
                    pure (current : rest)
