{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Parser where

import           Control.Monad               (foldM, foldM_, when)
import           Control.Monad.State.Strict  (MonadState (get, put),
                                              MonadTrans (lift),
                                              StateT (runStateT))
import           Data.Foldable               (traverse_)
import qualified Data.Function               as F
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (lexicalError, unbalancedDelimiter,
                                              withBlurb)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import           Data.Ouro.Internal.Utils    (rankBySimilarity)
import           Data.Ouro.Lisp.Function     (checkFunctionName, checkParams,
                                              duplicateFunction, functionBody,
                                              functionDoc, graphBody,
                                              isFunctionKeyword, patternParam,
                                              removedLocalDefn, renamedDefun,
                                              typedResult)
import           Data.Ouro.Lisp.Module.Types (Declaration (..),
                                              HigherExpression (..),
                                              Import (..), Module (..),
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


-- A simple compiler tracking state holding our remaining token stream
type ParseState = [Tkn.Token]

type Parser a = StateT ParseState (Either OuroError) a

-- Wraper for type indexed higher expressions
data ParsedDecl
    = PFunction (HigherExpression 'FunctionExpr)
    | PGraph    (HigherExpression 'GraphExpr)
    | PImport   Import
    | PExport   SourcePos [(Text, SourcePos)]


-- Top-level entry point
parseModule :: [Tkn.Token] -> Either OuroError Module
parseModule tokens = do
    (decls, _) <- runStateT (collectHigherExpressions []) tokens
    ouroModule <- buildModuleRegistry decls
    checkExports ouroModule
    pure ouroModule


-- Collection Pass (Building the structured env)
buildModuleRegistry :: [ParsedDecl] -> Either OuroError Module
buildModuleRegistry = foldM insertDecl emptyModule
    where
    emptyModule = Module Map.empty Map.empty [] []

    insertDecl :: Module -> ParsedDecl -> Either OuroError Module
    insertDecl m = \case
                    (PFunction t@(Function pos name _ _ _))
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
            lexicalError "Imports and exports come first in a file, before any defn or graph."
            F.& withBlurb "Perhaps move it to the top of the file, so the file's dependencies and exports are the first thing read."
            F.& OuroError pos
            F.& Left

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


-- Every exported name is a defn of this module, named once
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
            _noneClose                         -> "A module exports its own functions only, written with defn in the same file."


collectHigherExpressions :: [ParsedDecl] -> Parser [ParsedDecl]
collectHigherExpressions acc = do
    tokens <- get
    case tokens of
        []         -> pure (reverse acc) -- EOF reached safely
        _hasTokens -> do
                      decl <- pHigherExpression
                      collectHigherExpressions (decl : acc)


pHigherExpression :: Parser ParsedDecl
pHigherExpression = do
    startTkn <- popToken "Expected expression declaration starting with '('"
    case Tkn.tokenType startTkn of
        Tkn.OpenParen -> do
                         kwTkn <- popToken "Expected higher expression declaration keyword (defn, graph, import, export)"
                         case Tkn.tokenType kwTkn of
                             Tkn.Defn     -> pDefn (Tkn.pos startTkn)
                             Tkn.Defun    -> lift (Left (renamedDefun (Tkn.pos kwTkn)))
                             Tkn.Graph    -> pGraph (Tkn.pos startTkn)
                             Tkn.Import   -> pImport (Tkn.pos startTkn)
                             Tkn.Export   -> pExport (Tkn.pos startTkn)
                             other        -> lexicalError ("Invalid top-level keyword: " <> T.pack (show other))
                                             F.& OuroError (Tkn.pos kwTkn)
                                             F.& Left
                                             F.& lift
        other -> lexicalError ("Higher Expressions must begin with '('. Found: " <> T.pack (show other))
                 F.& OuroError (Tkn.pos startTkn)
                 F.& Left
                 F.& lift


pDefn :: SourcePos -> Parser ParsedDecl
pDefn pos = do
    name    <- expectSymbol "Expected a function name"
    retType <- pReturnType
    args    <- pArgs
    body    <- collectUntil Tkn.CloseParen

    -- A pattern parameter binds something, and each name is bound once
    lift (checkParams pos args)

    -- A (doc "...") among the body's direct elements is the function's documentation
    (doc, results) <- lift (functionDoc name body)

    -- A body is exactly one result expression, checked against the return type
    result <- lift (functionBody name pos results)

    pure $ PFunction (Function pos name doc args (S.Form pos [typedResult retType result]))


-- A type tag between a function's name and its parameters is its return type
pReturnType :: Parser (Maybe (SourcePos, S.ReaderTag))
pReturnType = do
    stream <- get
    case stream of
        t : ts | Just tag <- typeTag (Tkn.tokenType t)
            -> put ts >> pure (Just (Tkn.pos t, tag))
        _noReturnType
            -> pure Nothing


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
    stream <- get
    alias  <- case stream of
                  Tkn.Token _ (Tkn.Symbol "as") : Tkn.Token aliasPos (Tkn.Symbol name) : rest
                      -> do
                         put rest
                         lift (checkAlias aliasPos name)
                         pure (Just name)
                  _noAlias
                      -> pure Nothing
    closing <- popToken importForm
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
        t <- popToken "Expected ')' to close the export"
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


popToken :: Text -> Parser Tkn.Token
popToken err = do
    stream <- get
    case stream of
        []     -> lexicalError err
                  F.& OuroError (M.initialPos "unknown-source")
                  F.& Left
                  F.& lift
        (t:ts) -> put ts >> pure t


expectSymbol :: Text -> Parser Text
expectSymbol err = do
    t <- popToken err
    case Tkn.tokenType t of
        Tkn.Symbol name -> pure name
        _notASymbol     -> lexicalError err
                           F.& OuroError (Tkn.pos t)
                           F.& Left
                           F.& lift


expectCloseParen :: Parser ()
expectCloseParen = do
    t <- popToken "Expected closing ')'"
    case Tkn.tokenType t of
        Tkn.CloseParen -> pure ()
        _notAParen     -> unbalancedDelimiter "Expected ')'" "Found something else"
                          F.& OuroError (Tkn.pos t)
                          F.& Left
                          F.& lift


-- A parameter list: names or patterns, each optionally typed by the tag before it,
-- (#uri id type) or ((?slug ?label))
pArgs :: Parser [S.Param]
pArgs = do
    t <- popToken "Expected '(' for argument list"
    case Tkn.tokenType t of
        Tkn.OpenParen -> collectArgs []
        _other        -> lexicalError "Expected argument list starting with '('"
                         F.& OuroError (Tkn.pos t)
                         F.& Left
                         F.& lift

    where
    collectArgs acc = do
        t <- popToken "Expected argument or ')'"
        case Tkn.tokenType t of
            Tkn.CloseParen      -> pure $ reverse acc
            Tkn.Symbol     name -> collectArgs (S.Param name Nothing Nothing : acc)
            Tkn.OpenParen       -> do
                                   pattern <- parseFormContainer (Tkn.pos t)
                                   collectArgs (patternParam (length acc + 1) Nothing pattern : acc)
            tkn | Just tag <- typeTag tkn
                                -> do
                                   stream <- get
                                   case stream of
                                       next : rest | Tkn.tokenType next == Tkn.OpenParen
                                           -> do
                                              put rest
                                              pattern <- parseFormContainer (Tkn.pos next)
                                              collectArgs (patternParam (length acc + 1) (Just (Tkn.pos t, tag)) pattern : acc)
                                       _aName
                                           -> do
                                              name <- expectSymbol "A parameter's type is written before its name, as in (#num n)"
                                              collectArgs (S.Param name (Just (Tkn.pos t, tag)) Nothing : acc)
            _notAParameter      -> lexicalError "A parameter is a name, or a pattern in parentheses, each optionally typed: (#num n) or ((?a ?b))"
                                   F.& OuroError (Tkn.pos t)
                                   F.& Left
                                   F.& lift


-- The tags that name a type, in an expression or a signature. '#rec-empty' and
-- '#arr-empty' are values, not types.
typeTag :: Tkn.TokenType -> Maybe S.ReaderTag
typeTag = \case
    Tkn.TagUri  -> Just S.Uri
    Tkn.TagDate -> Just S.Date
    Tkn.TagStr  -> Just S.StrTag
    Tkn.TagNum  -> Just S.NumTag
    Tkn.TagBool -> Just S.BoolTag
    Tkn.TagRec  -> Just S.RecTag
    Tkn.TagArr  -> Just S.ArrTag
    Tkn.TagFn   -> Just S.FnTag
    _notAType   -> Nothing

-- Recursive Expr Router
pExpr :: Parser S.Expr
pExpr = do
        tokens <- get
        case tokens of
            -- For a sudden EOF, we generate an unclosed delimiter payload
            []     -> let pos     = M.initialPos "unknown-source"
                          context = Syntax UnbalancedDelimiter
                                        { expectedDelim = "Expression node layout component"
                                        , actualDelim   = "EOF (End of File)"
                                        }
                      in lift $ Left (OuroError pos context)
            (t:ts) -> let capture n = put ts >> pure n
                      in case Tkn.tokenType t of
                             -- Open boundaries push processing down into structural lookahead groups
                             -- Note: We intentionally pop 'ts' here to advance past the open delimiter
                             Tkn.OpenParen      -> put ts >> parseFormContainer (Tkn.pos t)
                             Tkn.OpenBracket    -> put ts >> parseBracketContainer (Tkn.pos t)

                             -- Primitive Leaf Node capture (+ source location)
                             Tkn.Let            -> capture (S.Symbol (Tkn.pos t) "let")
                             Tkn.Defn           -> capture (S.Symbol (Tkn.pos t) "defn")
                             Tkn.Defun          -> capture (S.Symbol (Tkn.pos t) "defun")

                             -- Parameterized Identifiers
                             Tkn.Symbol         txt -> capture (S.Symbol (Tkn.pos t) txt)

                             -- Attributes (Reader Macro -> `(%attr "key" expr)`)
                             Tkn.Attr           txt -> put ts >> parseAttrNode (Tkn.pos t) txt
                             Tkn.Binding        txt -> capture (binderNode (Tkn.pos t) txt)

                             -- Core Data Literals
                             Tkn.String txt     -> capture (S.Literal (Tkn.pos t) (S.Str txt))
                             Tkn.Number val     -> capture (S.Literal (Tkn.pos t) (S.Num val))
                             Tkn.Boolean b      -> capture (S.Literal (Tkn.pos t) (S.Bool b))
                             Tkn.Null           -> capture (S.Literal (Tkn.pos t) S.Null)

                             -- Reader Macros & Type Assertions
                             -- Note: We intentionally pop 'ts' here to advance past the macro tag
                             tkn | Just tag <- typeTag tkn
                                                -> put ts >> parseTaggedNode (Tkn.pos t) tag

                             -- Empty Collections instantiate as direct fallback values
                             Tkn.TagRecordEmpty -> capture (S.Literal (Tkn.pos t) S.EmptyRec)
                             Tkn.TagArrEmpty    -> capture (S.Literal (Tkn.pos t) S.EmptyArr)

                             -- Hole Type
                             Tkn.Hole txt       -> capture (S.Hole (Tkn.pos t) txt)

                             -- Unbalanced Boundaries are immediate semantic loop violations
                             Tkn.CloseParen -> let context = Syntax UnbalancedDelimiter
                                                                 { expectedDelim = "Opening Form Boundary '('"
                                                                 , actualDelim   = "Orphaned Closing Parenthesis ')'"
                                                                 }
                                               in lift $ Left (OuroError (Tkn.pos t) context)

                             Tkn.CloseBracket -> let context = Syntax UnbalancedDelimiter
                                                                   { expectedDelim = "Opening Array Boundary '['"
                                                                   , actualDelim   = "Orphaned Closing Bracket ']'"
                                                                   }
                                                 in lift $ Left (OuroError (Tkn.pos t) context)

                             _invalidToken -> lexicalError ("Invalid token: " <> (T.pack $ show (Tkn.tokenType t)) <> " found in expression")
                                              & OuroError (Tkn.pos t)
                                              & Left
                                              & lift


-- Lookahead Container Accumulators
parseFormContainer :: SourcePos -> Parser S.Expr
parseFormContainer startPos = do
                              terms <- collectUntil Tkn.CloseParen
                              pure $ S.Form startPos terms


-- Consumes tokens until a matching CloseBracket is found
parseBracketContainer :: SourcePos -> Parser S.Expr
parseBracketContainer startPos = do
                                 terms <- collectUntil Tkn.CloseBracket
                                 pure $ S.Bracket startPos terms


-- Reader Tag Helper: Verifies that the evaluated expression is a raw string payload
parseTaggedNode :: SourcePos -> S.ReaderTag -> Parser S.Expr
parseTaggedNode tagPos tagType = do
                                 stream <- get
                                 case stream of
                                     -- A property on its own, at the end of a form: (satisfies #str).
                                     -- Its subject is a hole that cannot be written in Ouro, filled
                                     -- in by the case it heads, and reported anywhere else
                                     t : _ | Tkn.CloseParen <- Tkn.tokenType t
                                         -> pure $ S.Tagged tagPos tagType (S.Hole tagPos "%subject")

                                     -- Otherwise the property applies to the expression after it
                                     _anExpression
                                         -> S.Tagged tagPos tagType <$> pExpr


-- Reader Macro Helper: Desugars attributes into `(%attr "name" expr)`. '%' cannot be
-- lexed, so the form cannot be written: an attribute pair is always written :key value
parseAttrNode :: SourcePos -> Text -> Parser S.Expr
parseAttrNode pos attrName = do
                             -- Eagerly consume the next expression to bind to the attribute
                             nextExpr <- pExpr
                             pure $ S.Form pos [ S.Symbol pos "%attr"
                                               , S.Literal pos (S.Str attrName)
                                               , nextExpr
                                               ]


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
    stream <- get
    case stream of
        -- The token stream ran dry before finding the matching delimiter.
        [] -> let pos     = M.initialPos "unknown-source" -- Or pass parent context position down
                  context = Syntax UnbalancedDelimiter
                              { expectedDelim = T.pack (show targetDelim)
                              , actualDelim   = "Unexpected EOF (End of File)"
                              }
              in lift $ Left (OuroError pos context)

        (t:ts) | Tkn.tokenType t == targetDelim -> do
                    put ts  -- Pop the matching closing token out of stream state
                    pure []
               | otherwise -> do
                    current <- pExpr
                    rest    <- collectUntil targetDelim
                    pure (current : rest)
