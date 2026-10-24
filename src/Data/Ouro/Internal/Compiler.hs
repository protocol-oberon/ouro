module Data.Ouro.Internal.Compiler
( CompilerT
, CompilerM
) where

import           Control.Monad.Validate (ValidateT)
import           Control.Monad.Writer   (WriterT)
import           Data.Functor.Identity  (Identity)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Ouro.Error.Types  (OuroError, OuroWarning)


-- The compiler's errors and warnings. Errors accumulate in ValidateT: work that does not
-- depend on another result (a traverse, <*>) reports every error it finds, while >>= stops
-- at the first, since the next step needs the value it did not get. So <*> collects errors
-- where ap would stop at the first. Warnings accumulate in the Writer underneath, so they
-- survive a failure. The base monad is Identity for compile, and IO for loading modules.
type CompilerT m = ValidateT (NonEmpty OuroError) (WriterT [OuroWarning] m)
type CompilerM   = CompilerT Identity
