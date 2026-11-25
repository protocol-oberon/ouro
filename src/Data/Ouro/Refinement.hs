-- Refinements: Ouro's second layer.
--
-- Layer 0 is data, JSON with parentheses, and the procedures that build it (Data.Ouro.Lisp).
-- Layer 1 is the sets over layer 0's values: which value is in which set, and why. A type is a
-- refinement by its name or one built from refinements with ∪ ∩ ∖, (array p) and
-- (record :k p …); a requirement is a type in a role, an argument, a result or an assertion;
-- and checking decides a value's membership, the evaluator running only a refinement defined
-- in Ouro, through the one call the checker is given.
module Data.Ouro.Refinement
    ( -- * Types
      module Data.Ouro.Refinement.Type
      -- * Requirements
    , module Data.Ouro.Refinement.Requirement
      -- * What a value is
    , module Data.Ouro.Refinement.Value
      -- * Checking
    , module Data.Ouro.Refinement.Check
    ) where

import           Data.Ouro.Refinement.Check
import           Data.Ouro.Refinement.Requirement
import           Data.Ouro.Refinement.Type
import           Data.Ouro.Refinement.Value
