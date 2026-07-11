
module Data.Ouro.Lisp.Surface where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Megaparsec (SourcePos)


-- Expr.
--
-- An untyped, ergonomically flexible Abstract Syntax Tree for the Ouro Lisp surface syntax.
-- Designed to prioritize lazy structural evaluation, recursive macro expansion, and developer ergonomics.
--
-- Unlike the rigid, type-indexed backbone of the internal core GADT, the surface AST is intentionally
-- untyped at the representation layer. This architectural omission of strict types allows the syntax
-- tree to support dynamic Lisp macros, forward variable referencing, and knot-tied lexical environment
-- mapping before any type verification or graph compilation passes occur.
--
-- We use uniform metadata anchoring (SourcePos) across all constructors to preserve physical text
-- coordinates from the lexer stream. This ensures that down-funnel type assertion failures and scope
-- lookup errors can map back to precise file locations during compilation.
--
-- Explicit token boundaries are maintained for identifiers and structural keys:
--   * Attr (:key) captures semantic keywords representing graph property transitions.
--   * Symbol (var) serves as an unbound lexical identifier, acting as a lookup hook for local variables,
--     parent frames, or native primitive operation registry fallbacks.
--
-- Atomic leaves are grouped homogeneously under Literal. By packaging raw values (Strings, Numbers,
-- Booleans, and Null structures) into an un-indexed payload wrapper, the parser can handle scalar
-- primitives flexibly as generic data tokens or macro arguments prior to static type inference.
--
-- Type assertions are injected via the Tagged constructor '#'. This allows for explicit validation of types
-- which may visually share identical lexical layouts under raw JSON-LD rules (such as Strings and URLs),
-- ensuring they can be contextualized and evaluated strictly according to intent by the compiler.
--
-- Sequence nesting relies on uniform, unrestricted collections:
--   * Form (...) provides a clean homoiconic list container where attribute definitions, macro
--     declarations, and function applications share a singular, highly flexible tree representation.
--   * Bracket [...] reserves an isolated structural grouping channel for vector layouts or multi-binding
--     scoping blocks like 'let' expressions.
--
-- The ultimate lifecycle of this surface AST is short-lived. It serves as an intermediate blueprint
-- completely consumed by the compilation Engine, which resolves its lazy environments, expands its
-- macro closures, and transforms it into the permanently typed, hyper-strict 'JLD.Type' core GADT.
data Expr
    = Symbol      SourcePos Text            -- variable names and functions
    | DefunSymbol SourcePos Text            -- template identifiers
    | Literal     SourcePos LiteralValue    -- Raw String, Number, Boolean, Null
    | Tagged      SourcePos ReaderTag Expr  -- #uri "...", #date "..."
    | Quoted      SourcePos Expr            -- Un-eval'd Asts
    | Hole        SourcePos Text            -- Type hole for structural shape
    | Form        SourcePos [Expr]          -- (...) nested lists
    | Bracket     SourcePos [Expr]          -- [...] scoping or block grouping
    deriving (Show, Eq)


-- Represents standard scalar literals and empty JSON data shapes
-- directly parsed from the Lisp source text.
data LiteralValue
    = Str  Text
    | Bool Bool
    | EmptyArr
    | EmptyRec
    | Null
    | Num Double
    deriving (Show, Eq)


-- Explicit semantic metadata tags used to enforce validation boundaries
-- or assert target GADT types during evaluation.
data ReaderTag
    = Uri
    | Date
    | StrTag   -- For explicit string enforcement (#str variable)
    | NumTag   -- For numeric assertion (#num variable)
    | BoolTag  -- For boolean assertion (#bool variable)
    | RecEmpty
    | ArrEmpty
    deriving (Show, Eq)


exprPos :: Expr -> SourcePos
exprPos = \case
           Symbol      pos _   -> pos
           DefunSymbol pos _   -> pos
           Literal     pos _   -> pos
           Tagged      pos _ _ -> pos
           Quoted      pos _   -> pos
           Hole        pos _   -> pos
           Form        pos _   -> pos
           Bracket     pos _   -> pos


structuralEq :: Expr -> Expr -> Maybe [(Text, Expr)]
structuralEq e1 e2 =
    case (e1, e2) of
        -- 1. Hole matching a single expression
        (Hole _ name, target)               -> bindHole name target
        (target     , Hole _ name)          -> bindHole name target

        -- 2. Exact leaf matches (return empty bindings on success)
        (Symbol  _ a, Symbol  _ b) | a == b -> Just []
        (Literal _ a, Literal _ b) | a == b -> Just []

        -- 3. Recursive matches
        (Quoted  _ a, Quoted   _ b)         -> structuralEq a b
        (Form    _ xs, Form    _ ys)        -> matchForms xs ys
        (Bracket _ xs, Bracket _ ys)        -> matchForms xs ys

        -- 4. Fallthrough: Mismatch
        _typeMismatch                       -> Nothing

    where
    -- Helper to capture the binding.
    -- Ignores pure wildcards ("?") so they don't pollute the environment.
    bindHole :: Text -> Expr -> Maybe [(Text, Expr)]
    bindHole holeName matchExpr =
        case (holeName, matchExpr) of
            ("?",  _)    -> Just []
            (name, expr) -> let cleanName = T.dropWhile (\c -> c == '?' || c == '.') name
                            in Just [(cleanName, expr)]

    matchForms :: [Expr] -> [Expr] -> Maybe [(Text, Expr)]
    matchForms xs ys =
        case (xs, ys) of
            -- Both empty: end of list reached simultaneously
            ([], [])
                -> Just []

            -- "?.." captures the rest of the expression
            (targetRemainder, Hole pos name : [])
                | T.isPrefixOf "?.." name
                  -> bindHole name (Form pos targetRemainder)

            -- Pure Positional Matching
            -- We remove the greedy 'Hole' catch here so that a list like
            -- (?expr ?plus) matches exactly two elements 1:1.
            (x:xs', y:ys')
                -> do
                   headBindings <- structuralEq x   y
                   tailBindings <- matchForms   xs' ys'
                   return (headBindings ++ tailBindings)

            _misMatchedLen
                -> Nothing

-- A runtime truth table to tell what shap of ast is present
data ExprMarker
    = AttrMarker
    | SymbolMarker
    | LiteralMarker
    | TaggedMarker
    | QuotedMarker
    | HoleMarker
    | FormMarker
    | BracketMarker
    deriving (Eq, Show)


mark :: Expr -> ExprMarker
mark = \case
        Symbol      _ _   -> SymbolMarker
        DefunSymbol _ _   -> SymbolMarker
        Literal     _ _   -> LiteralMarker
        Tagged      _ _ _ -> TaggedMarker
        Quoted      _ _   -> QuotedMarker
        Hole        _ _   -> HoleMarker
        Form        _ _   -> FormMarker
        Bracket     _ _   -> BracketMarker


matches :: ExprMarker -> ExprMarker -> Bool
matches = (==)
