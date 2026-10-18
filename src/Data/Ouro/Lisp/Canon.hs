
module Data.Ouro.Lisp.Canon
( Pass (..)
, canonical
, canonicalModule
, canonicalPattern
, passes
, patternPasses
, runPasses
) where

import           Data.Ouro.Lisp.Canon.Alternatives (alternatives)
import           Data.Ouro.Lisp.Canon.Bindings     (bindings)
import           Data.Ouro.Lisp.Canon.Comparisons  (comparisons)
import           Data.Ouro.Lisp.Canon.Context      (contexts)
import           Data.Ouro.Lisp.Canon.Guards       (guards)
import           Data.Ouro.Lisp.Canon.Operators    (operators)
import           Data.Ouro.Lisp.Canon.Placeholders (placeholders)
import           Data.Ouro.Lisp.Canon.Threading    (threading)
import           Data.Ouro.Lisp.Canon.Thru         (thru)
import           Data.Ouro.Lisp.Canon.With         (withForms)
import           Data.Ouro.Lisp.Module.Types       (Module, mapModuleExpr,
                                                    mapParamPatterns)
import qualified Data.Ouro.Lisp.Surface            as S
import           Data.Text                         (Text)


-- The passes.
--
-- Desugaring rewrites the surface tree into a smaller core, one rule per pass, in this order.
-- Each pass is a module of its own under Canon/, and walks the tree in one of the ways in
-- 'Canon.Traverse': top-down, bottom-up, or over lists of siblings.
--
--    1. placeholders  top-down   a call with '?' holes becomes a function of them
--    2. threading     top-down   (-> x step ...) threads x through its steps. A step with a
--                                hole is already a function (1), and a step is rewritten
--                                before 'with' is (6), so (-> x (with :k v)) gives with x
--    3. bindings      top-down   '$name value' in a let becomes (%bind "name" value), before a
--                                value can expand into several elements (7)
--    4. guards        top-down   a case guard ((< 10) e) is marked (%guard < 10), so that
--                                comparisons (8) and alternatives (10) leave it alone
--    5. contexts      bottom-up  ':context e' becomes the record's metadata, (%context e)
--    6. with          bottom-up  (with base item ...) becomes ((inlay base) item ...)
--    7. thru          siblings   (thru (days n)) becomes the durations it stands for
--    8. comparisons   bottom-up  (< a b c) becomes (%and (< a b) (< b c))
--    9. operators     bottom-up  (+ a b c) becomes (+ (+ a b) c)
--   10. alternatives  bottom-up  a case branch (("a" "b") e) becomes ("a" e) ("b" e)
data Pass = Pass
    { passName :: Text
    , runPass  :: S.Expr -> S.Expr
    }

passes :: [Pass]
passes =
    [ Pass "placeholders" placeholders
    , Pass "threading"    threading
    ] <> patternPasses


-- The passes a defn's parameter patterns go through: all but the first two, since a
-- pattern's holes match values, so they are neither placeholders nor threaded
patternPasses :: [Pass]
patternPasses =
    [ Pass "bindings"     bindings
    , Pass "guards"       guards
    , Pass "contexts"     contexts
    , Pass "with"         withForms
    , Pass "thru"         thru
    , Pass "comparisons"  comparisons
    , Pass "operators"    operators
    , Pass "alternatives" alternatives
    ]


-- Runs the given passes, in order
runPasses :: [Pass] -> S.Expr -> S.Expr
runPasses ps expr = foldl (\e p -> runPass p e) expr ps

-- Runs every pass
canonical :: S.Expr -> S.Expr
canonical = runPasses passes

-- Runs the passes for a parameter pattern
canonicalPattern :: S.Expr -> S.Expr
canonicalPattern = runPasses patternPasses

-- Desugars a module: every function's body and every graph, and every defn's parameter patterns
canonicalModule :: Module -> Module
canonicalModule = mapModuleExpr canonical . mapParamPatterns canonicalPattern
