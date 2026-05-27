module Data.Oberon.Lisp.Parser where

import           Control.Monad.State.Strict
import qualified Data.Oberon.Lisp.Surface   as S
import qualified Data.Oberon.Lisp.Tokens    as Tkn
import           Text.Megaparsec            (SourcePos)

-- A simple compiler tracking state holding our remaining token stream
type ParseState = [Tkn.Token]

type Parser a = StateT ParseState (Either String) a


-- Top-level entry point
parse :: [Tkn.Token] -> Either String S.Expr
parse tokens = do
            (expr, remaining) <- runStateT pExpr tokens
            case remaining of
                []    -> Right expr
                (t:_) -> Left $ "Unexpected trailing token at line "
                             ++ show (Tkn.pos t)
                             ++ ": "
                             ++ show (Tkn.tokenType t)


-- Recursive Expr Router
pExpr :: Parser S.Expr
pExpr = do
        tokens <- get
        case tokens of
            []     -> lift $ Left "Unexpected End of File while parsing expression."
            (t:ts) -> let capture n = put ts >> pure n
                      in case Tkn.tokenType t of
                             -- Open boundaries push processing down into structural lookahead groups
                             -- Note: We intentionally pop 'ts' here to advance past the open delimiter!
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

                             -- Unbalanced Boundaries are immediate semantic loop violations
                             Tkn.CloseParen     -> lift $ Left $ "Mismatched closing parenthesis at " ++ show (Tkn.pos t)
                             Tkn.CloseBracket   -> lift $ Left $ "Mismatched closing bracket at "     ++ show (Tkn.pos t)


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
                                 -- (Could be a string literal, symbol, or list form!)
                                 nextExpr <- pExpr

                                 -- Construct the Tagged node using the expression directly!
                                 pure $ S.Tagged tagPos tagType nextExpr


-- Parsing Stream State Helpers
collectUntil :: Tkn.TokenType -> Parser [S.Expr]
collectUntil targetDelim = do
                           stream <- get
                           case stream of
                               []     -> lift $ Left $ "Missing closing delimiter: expected " ++ show targetDelim
                               (t:ts) | Tkn.tokenType t == targetDelim -> do
                                            put ts  -- Pop closing token out of stream state
                                            pure []
                                      | otherwise -> do
                                            current <- pExpr
                                            rest    <- collectUntil targetDelim
                                            pure (current : rest)
