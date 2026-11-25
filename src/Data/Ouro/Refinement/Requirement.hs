module Data.Ouro.Refinement.Requirement where

import           Data.Ouro.Refinement.Type (Type)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Text.Megaparsec           (SourcePos)


-- Role.
--
-- What a value was required for, where a requirement is checked. Who is blamed follows from
-- it: an argument's caller, a result's function, an assertion itself.
data Role
    = ArgumentOf Callee Int  -- The nth argument of a call, counted from 1
    | ResultOf   Callee      -- What a function returns, under its return type
    | Assertion              -- <x ⊢ p>, a statement written in Ouro
    deriving (Show, Eq, Ord, Generic)


-- The function a requirement belongs to, as it is called: how it is identified, never what
-- kind it is, so a procedure and a refinement are named alike
data Callee
    = Defined Text  -- A defined function, by its name as written: a procedure or a refinement
    | Lambda        -- A fn
    deriving (Show, Eq, Ord, Generic)


-- Who a failure is reported against: the two sides of a call, and an assertion alone
data Party
    = Caller         -- Whoever called the function, for an argument it supplied
    | Called Callee  -- The function called, for the result it made
    | Itself         -- The assertion, which only its own expression can break
    deriving (Show, Eq, Ord, Generic)


-- Blame, read off the role, never stored: an argument is supplied by the caller, so a failed
-- argument blames the caller; a result is made by the function called, so a failed result
-- blames it; an assertion has one party. Findler and Felleisen's split of a function type into
-- its negative and positive positions.
blame :: Role -> Party
blame = \case
    ArgumentOf _ _  -> Caller
    ResultOf callee -> Called callee
    Assertion       -> Itself


-- Failure.
--
-- A value outside a requirement, as data: what was required and where it is written, a sketch
-- of the value given, the role it was required in, and the trail from the requirement down to
-- the part that failed. The error renders it as the failing branch of the check tree; an
-- interactive view renders the whole tree from the same data. Blame is derived from the role,
-- never stored.
data Failure = Failure
    { required   :: Type        -- The type as written: (searched-nothing-in dealer-ledgers)
    , requiredAt :: SourcePos   -- Where it is written: a binder, a return type, an assertion
    , given      :: Text        -- A sketch of the value
    , role       :: Role        -- What the value was required for
    , trail      :: [Step]      -- From the requirement down to the part that failed, outermost first
    , rule       :: Maybe Text  -- The failing refinement's doc, the rule it states
    }
    deriving (Show, Eq, Ord, Generic)


-- One boundary a failure crossed on its way out: where, which part of the value, and the type
-- checked there
data Step = Step
    { stepAt   :: SourcePos
    , stepPart :: Part
    , stepType :: Type
    }
    deriving (Show, Eq, Ord, Generic)


-- Which part of a value a step checked
data Part
    = Element Int  -- An array's element, counted from 1
    | Key Text     -- A record's value at a key
    | Body Text    -- Inside a refinement's body, by the refinement's name
    | Whole        -- The value itself
    deriving (Show, Eq, Ord, Generic)
