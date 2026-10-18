
module Data.Ouro.Lisp.Canon where

import           Data.List              (mapAccumL)
import qualified Data.Ouro.Lisp.Surface as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Megaparsec        (SourcePos)


-- placeholders.
--
-- A call with '?' holes among its arguments is a function of its holes:
--
--   (+ ? 1)          (fn (%1) (+ %1 1))
--   (node ? "Type")  (fn (%1) (node %1 "Type"))
--   (* ?n ?n)        (fn (?n) (* ?n ?n))       a named hole is one parameter, however often it is written
--   (f #num ? x)     (fn (#num %1) (f %1 x))   a tagged hole is a typed parameter
--
-- A hole belongs to the call it is an argument of, so (f (g ?)) passes f a function. The
-- parameters are the holes in the order they are first written. Their names cannot be
-- written in Ouro ('%1', and '?n', which lexes as a hole), so they never capture a name
-- the call refers to. A case pattern's holes match values instead, so patterns are left
-- as they are, and so are a fn's parameters, which can be patterns too. A rest hole
-- ('?..name') belongs only in a pattern, and is reported by the evaluator wherever else
-- it is written. Runs before 'construct', so the function's body is desugared like any
-- other call.
placeholders :: S.Expr -> S.Expr
placeholders = \case
    S.Form pos (cSym@(S.Symbol _ "case") : target : branches)
        -> S.Form pos (cSym : placeholders target : map branch branches)

    -- A fn's parameters are names and patterns, not arguments, so only its body is read
    S.Form pos [fnSym@(S.Symbol _ "fn"), params, body]
        -> S.Form pos [fnSym, params, placeholders body]

    S.Form pos (headExpr@(S.Symbol _ name) : args)
        | name `notElem` holeFreeForms
        , not (any S.isAttrForm args)
        , any hole args
        -> holeFunction pos headExpr (map placeholders args)

    S.Form    pos items     -> S.Form    pos (map placeholders items)
    S.Bracket pos items     -> S.Bracket pos (map placeholders items)
    S.Tagged  pos tag inner -> S.Tagged  pos tag (placeholders inner)
    leaf                    -> leaf

    where
    -- Only a branch's body: its pattern is matched, not called
    branch = \case
        S.Form bPos [pattern, body] -> S.Form bPos [pattern, placeholders body]
        malformed                   -> malformed


-- Forms with their own structure, whose arguments are not values a hole could stand for
holeFreeForms :: [Text]
holeFreeForms = [ "attr", "case", "context", "define", "defn", "defun", "fn", "graph"
                , "inlay", "let", "thru", "%binder" ]


-- An argument that is a hole, or a hole under a type tag. A rest hole is not a parameter.
hole :: S.Expr -> Bool
hole = \case
    S.Hole _ name                  -> not ("?.." `T.isPrefixOf` name)
    S.Tagged _ _ inner@(S.Hole _ _) -> hole inner
    _notAHole                      -> False


-- Builds (fn (params) (head args)) from a call with holes
holeFunction :: SourcePos -> S.Expr -> [S.Expr] -> S.Expr
holeFunction pos headExpr args =
    S.Form pos [ S.Symbol pos "fn"
               , S.Form pos (firstOfEach [ p | Just p <- params ])
               , S.Form pos (headExpr : args')
               ]
    where
    (args', params) = unzip (snd (mapAccumL fill (1 :: Int) args))

    -- Each anonymous hole is a new parameter, named by its position among them
    fill n = \case
        S.Hole hPos "?"
            -> let ref = S.Symbol hPos ("%" <> T.pack (show n))
               in (n + 1, (ref, Just ref))
        arg@(S.Hole hPos name)
            | hole arg
            -> let ref = S.Symbol hPos name
               in (n, (ref, Just ref))
        arg@(S.Tagged tPos tag inner)
            | hole arg
            -> let (n', (ref, _)) = fill n inner
               in (n', (ref, Just (S.Tagged tPos tag ref)))
        arg
            -> (n, (arg, Nothing))

    -- A named hole written twice is one parameter: the first, with its type if it has one
    firstOfEach = foldr (\p kept -> p : filter ((/= paramName p) . paramName) kept) []
    paramName = \case
        S.Tagged _ _ ref -> paramName ref
        S.Symbol _ name  -> name
        _notAParam       -> ""


-- Desugars an expression that stands on its own, such as a graph's body or the payload
-- of a type tag. 'construct' rewrites the elements of a form, so the expression itself
-- is desugared as the only element of a list. Sugar that expands into several elements,
-- like a bare (thru ...), has no meaning on its own and is left for the evaluator to report.
constructRoot :: S.Expr -> S.Expr
constructRoot expr =
    case desugar [construct expr] of
        [root]         -> root
        _expandedSugar -> construct expr


-- Main API entry point for the pipeline
construct :: S.Expr -> S.Expr
construct = \case
             -- Guards and let bindings are prepared before the children are desugared, bottom-up
             S.Form    r   exprs -> S.Form    r   (desugar (map construct (prepare exprs)))
             S.Bracket r   exprs -> S.Bracket r   (desugar (map construct exprs))
             S.Tagged  r t expr  -> S.Tagged  r t (constructRoot expr)

             node@(S.Symbol      _ _) -> node
             node@(S.DefunSymbol _ _) -> node
             node@(S.Literal     _ _) -> node
             node@(S.Hole        _ _) -> node


-- Processes flat lists horizontally to rewrite syntax sugar
desugar :: [S.Expr] -> [S.Expr]
desugar = \case
           [] -> []

           -- A record's context, ':context value', is its metadata rather than an attribute
           -- pair: (%context value), whose value is read as a JSON-LD context when the record
           -- is built. ':@context', JSON-LD's own name for it, is the same. '%' cannot be
           -- written in Ouro, so the head cannot be confused with a user's function.
           (S.Form pos [S.Symbol _ "attr", S.Literal _ (S.Str key), valExpr] : rest)
               | key `elem` ["context", "@context"]
               -> S.Form pos [S.Symbol pos "%context", valExpr] : desugar rest

           -- Desugar end-of to max temporal duration for given duration type
           (S.Form tPos [S.Symbol _ "thru", durationExpr] : rest)
               -> desugarThru tPos durationExpr ++ desugar rest

           -- Deep Recurse: Bottom-Up evaluation pass
           (S.Form pos items : xs)
               -> let desugaredItems = desugar (prepare items)    -- Expand 'thru' and desugar children FIRST
                      unrolled       = desugarCase             -- Expand multi-match cases
                                     $ desugarBinOp            -- Unroll variadic ops
                                     $ desugarComparison       -- Unroll comparisons
                                     $ S.Form pos desugaredItems
                  in unrolled : desugar xs

           -- Horizontal Pass-through (Safely passes over Tags, Symbols, and Literals)
           (x : xs) -> x : desugar xs


-- Unrolls variadic comparisons into nested binary 'and' checks.
-- E.g., (eq 1 2 3) -> (%and (eq 1 2) (eq 2 3))
--
-- '%and' is an internal builtin: '%' cannot be lexed in user symbols or keys,
-- so the desugared form can never be shadowed by a user binding.
desugarComparison :: S.Expr -> S.Expr
desugarComparison =
    \case
     -- Base Case: Exactly two arguments. Return as-is.
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
         | opName `elem` ["eq", ">", "<", ">=", "<="]
             -> S.Form pos [opNode, a, b]

     -- Recursive Case: Three or more arguments. Chain them.
     S.Form pos (opNode@(S.Symbol sPos opName) : a : b : xs)
         | opName `elem` ["eq", ">", "<", ">=", "<="]
             -> let leftCheck  = S.Form pos [opNode, a, b]
                    rightCheck = desugarComparison (S.Form pos (opNode : b : xs))
                in S.Form pos [S.Symbol sPos "%and", leftCheck, rightCheck]

     -- Pass-through for anything that isn't a comparison
     otherVal
         -> otherVal


-- Unrolls variadic arithmetic into nested, left-associative binary operations.
-- E.g., (+ 1 2 3) -> (+ (+ 1 2) 3)
desugarBinOp :: S.Expr -> S.Expr
desugarBinOp =
    \case
     -- Base Case: Exactly two arguments. Return as-is
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : [])
         | opName `elem` ["+", "-", "*", "/"]
             -> S.Form pos [opNode, a, b]

     -- Recursive Case: Three or more arguments. Left-associative fold.
     S.Form pos (opNode@(S.Symbol _ opName) : a : b : xs)
         | opName `elem` ["+", "-", "*", "/"]
             -> let inner = S.Form pos [opNode, a, b]
                in desugarBinOp (S.Form pos (opNode : inner : xs))

     -- Pass-through for anything that isn't a variadic math operation
     otherVal
         -> otherVal


desugarThru :: SourcePos -> S.Expr -> [S.Expr]
desugarThru pos target = case target of
    -- Calendar Units: Need Runtime Checks.
    S.Form _pos [S.Symbol _ "years", amt]  -> [ S.Form pos [S.Symbol pos "years-end", amt] ]
    S.Form _pos [S.Symbol _ "months", amt] -> [ S.Form pos [S.Symbol pos "months-end", amt] ]

    -- Absolute Units: Can be statically expanded.
    S.Form _pos [S.Symbol _ "days", _amt]
        -> [ target
           , mkDur "hours" 23
           , mkDur "minutes" 59
           , mkDur "seconds" 59
           ]

    S.Form _pos [S.Symbol _ "hours", _amt]
        -> [ target
           , mkDur "minutes" 59
           , mkDur "seconds" 59
           ]

    S.Form _pos [S.Symbol _ "minutes", _amt]
        -> [ target
           , mkDur "seconds" 59
           ]

    -- Catch malformed syntax and leave it for the runtime type-checker
    other -> [other]

    where
    mkDur unit val = S.Form pos [ S.Symbol pos unit, S.Literal pos (S.Num val) ]


-- Rewrites that must happen before a form's parts are desugared, because they depend on
-- the form's own shape: case guards, and the bindings of a let
prepare :: [S.Expr] -> [S.Expr]
prepare = pairBindings . markGuards


-- Pairs each '$name' in a let's binding list with the value after it:
-- (let ($a 1 $b 2) body) becomes (let ((%bind "a" 1) (%bind "b" 2)) body). Pairing comes
-- first, so a value that desugars into several elements stays inside its binding. A name
-- with no value after it is left as (%bind "name"), which the evaluator reports.
pairBindings :: [S.Expr] -> [S.Expr]
pairBindings = \case
    letSym@(S.Symbol _ "let") : S.Form lPos bindings : rest -> letSym : S.Form lPos (pairUp bindings) : rest
    other                                                   -> other

    where
    pairUp = \case
        S.Form bPos [S.Symbol sPos "%binder", name] : value : more
            | not (isBinder value)
            -> S.Form bPos [S.Symbol sPos "%bind", name, value] : pairUp more
        S.Form bPos [S.Symbol sPos "%binder", name] : more
            -> S.Form bPos [S.Symbol sPos "%bind", name] : pairUp more
        item : more
            -> item : pairUp more
        []
            -> []

    isBinder = \case
        S.Form _ (S.Symbol _ "%binder" : _) -> True
        _notABinder                         -> False


-- Marks the guards of a case form before its parts are desugared. A guard such as
-- ((< 10 20) body) compares the case's target with its operands, (< target 10 20), so
-- it must not be desugared as a comparison of its own operands. The head becomes
-- (%guard < 10 20); '%' cannot be written in Ouro, so the marker cannot be shadowed.
markGuards :: [S.Expr] -> [S.Expr]
markGuards = \case
    cSym@(S.Symbol _ "case") : target : branches -> cSym : target : map markBranch branches
    other                                        -> other

    where
    markBranch = \case
        S.Form bPos [S.Form gPos (opNode@(S.Symbol sPos op) : operands), body]
            | op `elem` guardOperators
            -> S.Form bPos [S.Form gPos (S.Symbol sPos "%guard" : opNode : operands), body]
        branch
            -> branch


-- The comparisons that can head a case guard
guardOperators :: [Text]
guardOperators = [">=", ">", "<=", "<", "eq", "neq"]


desugarCase :: S.Expr -> S.Expr
desugarCase =
    \case
     -- Intercept 'case' form
     S.Form fPos (cSym@(S.Symbol _ "case") : target : branches)
         -> let desugaredTarget   = desugarCase target
                desugaredBranches = concatMap expandBranch branches
            in S.Form fPos (cSym : desugaredTarget : desugaredBranches)
     other -> other

    where
    -- Expands grouped patterns into individual branches
    expandBranch :: S.Expr -> [S.Expr]
    expandBranch = \case
                    -- Deconstruct a valid branch shape
                    S.Form bPos [pattern, body]
                        -> let desugaredBody = desugarCase body
                           in case pattern of
                                  -- Keep guards intact (e.g., (> 5)), marked by 'markGuards'
                                  S.Form _ (S.Symbol _ "%guard" : _)
                                      -> [S.Form bPos [pattern, desugaredBody]]

                                  -- Keep 'otherwise' intact
                                  S.Symbol _ "otherwise"
                                      -> [S.Form bPos [pattern, desugaredBody]]

                                  -- Keep destructuring patterns intact, e.g. ((?head ?..rest) body)
                                  _ | S.isPatternForm pattern
                                      -> [S.Form bPos [pattern, desugaredBody]]

                                  -- THE DESUGARING STEP:
                                  -- Example: ( ("Manet" "Proust") body ) -> ( "Manet" body ) ( "Proust" body )
                                  S.Form _ subPatterns
                                      -> [S.Form bPos [subPat, desugaredBody] | subPat <- subPatterns]

                                  -- Keep single literals or undefined symbols intact
                                  _singleLitOrSym -> [S.Form bPos [pattern, desugaredBody]]

                    -- If a branch is malformed, pass it through unchanged so `evaluateGuards`
                    -- can catch it and throw the proper `malformedCaseBranch` error with SourcePos.
                    malformed -> [malformed]
