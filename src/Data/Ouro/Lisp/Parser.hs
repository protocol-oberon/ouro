module Data.Ouro.Lisp.Parser where

import           Control.Monad.State.Strict (MonadState (get, put),
                                             MonadTrans (lift),
                                             StateT (runStateT))
import           Data.Ouro.Error.Types      (ErrorContext (..), OuroError (..),
                                             SyntaxError (..))
import qualified Data.Ouro.Lisp.Surface     as S
import qualified Data.Ouro.Lisp.Tokens      as Tkn
import qualified Data.Text                  as T
import           Text.Megaparsec            (SourcePos)
import qualified Text.Megaparsec.Pos        as M


-- A simple compiler tracking state holding our remaining token stream
type ParseState = [Tkn.Token]

type Parser a = StateT ParseState (Either OuroError) a


-- Top-level entry point
parse :: [Tkn.Token] -> Either OuroError S.Expr
parse tokens = do
    (expr, remaining) <- runStateT pExpr tokens
    case remaining of
        []    -> Right expr
        (t:_) -> let pos     = Tkn.pos t
                     tType   = Tkn.tokenType t
                     context = Syntax UnbalancedDelimiter
                                 { expectedDelim = "EOF (End of File) or structural closing boundary"
                                 , actualDelim   = T.pack (show tType)
                                 }
                 in Left (OuroError pos context)


-- Recursive Expr Router
pExpr :: Parser S.Expr
pExpr = do
        tokens <- get
        case tokens of
            -- For a sudden EOF, we generate an unclosed delimiter payload
            []     -> let pos     = M.initialPos "unknown-source" -- Or pass down the last known token's position
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
                             Tkn.Context        -> capture (S.Symbol (Tkn.pos t) "context")
                             Tkn.FlagVocab      -> capture (S.Attr   (Tkn.pos t) "vocab")
                             Tkn.FlagLanguage   -> capture (S.Attr   (Tkn.pos t) "language")
                             Tkn.FlagBase       -> capture (S.Attr   (Tkn.pos t) "base")
                             Tkn.FlagTerms      -> capture (S.Attr   (Tkn.pos t) "terms")
                             Tkn.FlagClear      -> capture (S.Attr   (Tkn.pos t) "clear")
                             Tkn.TypeMappingID  -> capture (S.Attr   (Tkn.pos t) "id")
                             Tkn.TypeMappingIRI -> capture (S.Attr   (Tkn.pos t) "iri")

                             -- Parameterized Identifiers
                             Tkn.Attr txt       -> capture (S.Attr   (Tkn.pos t) txt)
                             Tkn.Symbol txt     -> capture (S.Symbol (Tkn.pos t) txt)

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
                             Tkn.TagObjectEmpty -> capture (S.Literal (Tkn.pos t) S.EmptyObj)
                             Tkn.TagArrEmpty    -> capture (S.Literal (Tkn.pos t) S.EmptyArr)

                             -- Quoted Expr
                             Tkn.Quote          -> put ts >> parseQuotedNode (Tkn.pos t)

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
