{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Data.Ouro.Lisp.Parser where

import           Control.Monad               (foldM)
import           Control.Monad.State.Strict  (MonadState (get, put),
                                              MonadTrans (lift),
                                              StateT (runStateT))
import           Data.Foldable               (traverse_)
import qualified Data.Function               as F
import qualified Data.Map                    as Map
import           Data.Ouro.Error.Diagnostics (lexicalError, unbalancedDelimiter)
import           Data.Ouro.Error.Types       (ErrorContext (..), OuroError (..),
                                              SyntaxError (..))
import           Data.Ouro.Lisp.Module.Types (Declaration (..),
                                              HigherExpression (..),
                                              Module (..), functionRegistry,
                                              graphRegistry, templateRegistry)
import qualified Data.Ouro.Lisp.Surface      as S
import qualified Data.Ouro.Lisp.Tokens       as Tkn
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro.Platform         (at, (&), (?~))
import           Text.Megaparsec             (SourcePos)
import qualified Text.Megaparsec.Pos         as M


-- A simple compiler tracking state holding our remaining token stream
type ParseState = [Tkn.Token]

type Parser a = StateT ParseState (Either OuroError) a

-- Wraper for type indexed higher expressions
data ParsedDecl
    = PFunction (HigherExpression 'FunctionExpr)
    | PTemplate (HigherExpression 'TemplateExpr)
    | PGraph    (HigherExpression 'GraphExpr)


-- Top-level entry point
parseModule :: [Tkn.Token] -> Either OuroError Module
parseModule tokens = do
    (decls, _) <- runStateT (collectHigherExpressions []) tokens
    buildModuleRegistry decls


-- Collection Pass (Building the structured env)
buildModuleRegistry :: [ParsedDecl] -> Either OuroError Module
buildModuleRegistry = foldM insertDecl emptyModule
    where
    emptyModule = Module Map.empty Map.empty Map.empty Map.empty

    insertDecl :: Module -> ParsedDecl -> Either OuroError Module
    insertDecl m = \case
                    (PFunction d@(Function _ name _ _))  -> Right $ m & functionRegistry . at name ?~ d
                    (PTemplate t@(Template _ name _ _))  -> Right $ m & templateRegistry . at name ?~ t
                    (PGraph    g@(Graph    _ name body)) -> do
                                                            validateGraphBody body
                                                            Right $ m & graphRegistry    . at name ?~ g

    -- Semantic validation
    validateGraphBody :: S.Expr -> Either OuroError ()
    validateGraphBody =
        \case
         S.Form pos (S.Symbol _ "defun"    : _) -> lexicalError "Graphs cannot contain nested 'defun' declarations."
                                                   F.& OuroError pos
                                                   F.& Left
         S.Form pos (S.Symbol _ "template" : _) -> lexicalError "Graphs cannot contain nested 'template' declarations."
                                                   F.& OuroError pos
                                                   F.& Left
         S.Form _   exprs                       -> traverse_ validateGraphBody exprs
         _safeLeafs                             -> Right ()


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
                         kwTkn <- popToken "Expected higher expression declaration keyword (defun, template, graph)"
                         case Tkn.tokenType kwTkn of
                             Tkn.Defun    -> pFunction (Tkn.pos startTkn)
                             Tkn.Template -> pTemplate (Tkn.pos startTkn)
                             Tkn.Graph    -> pGraph    (Tkn.pos startTkn)
                             other        -> lexicalError ("Invalid top-level keyword: " <> T.pack (show other))
                                             F.& OuroError (Tkn.pos kwTkn)
                                             F.& Left
                                             F.& lift
        other -> lexicalError ("Higher Expressions must begin with '('. Found: " <> T.pack (show other))
                 F.& OuroError (Tkn.pos startTkn)
                 F.& Left
                 F.& lift


pFunction :: SourcePos -> Parser ParsedDecl
pFunction pos = do
    name <- expectSymbol "Expected function name"
    args <- pArgs
    body <- pExpr
    expectCloseParen
    pure $ PFunction (Function pos name args body)


pTemplate :: SourcePos -> Parser ParsedDecl
pTemplate pos = do
    -- We specifically enforce the TemplateSymbol (e.g., ends in '!')
    name <- expectTemplateSymbol "Expected template name ending with '!'"
    args <- pArgs
    body <- collectUntil Tkn.CloseParen
    pure $ PTemplate (Template pos name args (S.Form pos body))


pGraph :: SourcePos -> Parser ParsedDecl
pGraph pos = do
    name <- expectSymbol "Expected graph name"
    body <- collectUntil Tkn.CloseParen
    pure $ PGraph (Graph pos name (S.Form pos body))


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


expectTemplateSymbol :: Text -> Parser Text
expectTemplateSymbol err = do
    t <- popToken err
    case Tkn.tokenType t of
        Tkn.TemplateSymbol name -> pure name
        _notATemplateSymbol     -> lexicalError err
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


pArgs :: Parser [Text]
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
            Tkn.Symbol     name -> collectArgs (name : acc)
            _notASymbol         -> lexicalError "Argument lists can only contain symbols"
                                   F.& OuroError (Tkn.pos t)
                                   F.& Left
                                   F.& lift

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
                             Tkn.Template       -> capture (S.Symbol (Tkn.pos t) "template")
                             Tkn.Defun          -> capture (S.Symbol (Tkn.pos t) "defun")
                             Tkn.FlagVocab      -> capture (S.Attr   (Tkn.pos t) "vocab")
                             Tkn.FlagLanguage   -> capture (S.Attr   (Tkn.pos t) "language")
                             Tkn.FlagBase       -> capture (S.Attr   (Tkn.pos t) "base")
                             Tkn.FlagTerms      -> capture (S.Attr   (Tkn.pos t) "terms")
                             Tkn.FlagClear      -> capture (S.Attr   (Tkn.pos t) "clear")
                             Tkn.TypeMappingID  -> capture (S.Attr   (Tkn.pos t) "id")
                             Tkn.TypeMappingIRI -> capture (S.Attr   (Tkn.pos t) "iri")

                             -- Parameterized Identifiers
                             Tkn.Attr           txt -> capture (S.Attr   (Tkn.pos t) txt)
                             Tkn.Symbol         txt -> capture (S.Symbol (Tkn.pos t) txt)
                             Tkn.TemplateSymbol txt -> capture (S.Symbol (Tkn.pos t) txt)

                             -- Core Data Literals
                             Tkn.String txt     -> capture (S.Literal (Tkn.pos t) (S.Str txt))
                             Tkn.Number val     -> capture (S.Literal (Tkn.pos t) (S.Num val))
                             Tkn.Boolean b      -> capture (S.Literal (Tkn.pos t) (S.Bool b))
                             Tkn.Null           -> capture (S.Literal (Tkn.pos t) S.Null)

                             -- Reader Macros & Type Assertions
                             -- Note: We intentionally pop 'ts' here to advance past the macro tag
                             Tkn.TagUri         -> put ts >> parseTaggedNode (Tkn.pos t) S.Uri
                             Tkn.TagDate        -> put ts >> parseTaggedNode (Tkn.pos t) S.Date
                             Tkn.TagStr         -> put ts >> parseTaggedNode (Tkn.pos t) S.StrTag
                             Tkn.TagNum         -> put ts >> parseTaggedNode (Tkn.pos t) S.NumTag
                             Tkn.TagBool        -> put ts >> parseTaggedNode (Tkn.pos t) S.BoolTag

                             -- Empty Collections instantiate as direct fallback values
                             Tkn.TagRecordEmpty -> capture (S.Literal (Tkn.pos t) S.EmptyRec)
                             Tkn.TagArrEmpty    -> capture (S.Literal (Tkn.pos t) S.EmptyArr)

                             -- Quoted Expr
                             Tkn.Quote          -> put ts >> parseQuotedNode (Tkn.pos t)

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
                                 -- Dynamically pull the next full S-expression in the stream
                                 -- (Could be a string literal, symbol, or list form)
                                 nextExpr <- pExpr

                                 -- Construct the Tagged node using the expression directly
                                 pure $ S.Tagged tagPos tagType nextExpr

parseQuotedNode :: SourcePos -> Parser S.Expr
parseQuotedNode startPos = do
                           nextExpr <- pExpr
                           pure $ S.Quoted startPos nextExpr


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
