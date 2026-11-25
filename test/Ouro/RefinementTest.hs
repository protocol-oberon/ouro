module Ouro.RefinementTest
( requirements
, kinds
) where

import qualified Data.Ouro.Internal.Expr   as I
import           Data.Ouro.Lisp.Eval.Types (Expr (..))
import qualified Data.Ouro.Refinement      as R
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Hedgehog
import           Utils                     (compilesToJSON, failsWith, warnsWith,
                                            warnsWithout)


-- A value outside a requirement is a Refinement Failed, which keeps, as data, what was required,
-- the value given and its kind, the role it was required in, and the trail from the requirement
-- down to the part that failed. Who is blamed is read off the role. These tests read the failure
-- as it is shown, so a field renamed is a test that fails, on purpose.
requirements :: Group
requirements = Group "Refinements: a failure, its role and its trail"
    [ -- The role: an argument, a result, an assertion
      ("an argument's role names the call and the position", failsWith "role = ArgumentOf (Defined \"double\") 1"
                                                              "(define/procedure double (<n ⊢ number>) (* n 2)) (document g (:r (double \"x\")))")
    , ("as a fn's does, unnamed",                     failsWith "role = ArgumentOf Lambda 1"
                                                              "(document g (:r (let ($f (fn (<n ⊢ number>) n)) (f \"x\"))))")
    , ("a result's role names the function",          failsWith "role = ResultOf (Defined \"label\")"
                                                              "(define/procedure label ⊢ number (who) who) (document g (:r (label \"a\")))")
    , ("an assertion's role is itself",               failsWith "role = Assertion"
                                                              "(document g (:a <1 ⊢ string>))")
    , ("the value given is kept with its kind",       failsWith "given = \"string\", givenKind = Just RString"
                                                              "(define/procedure double (<n ⊢ number>) (* n 2)) (document g (:r (double \"x\")))")

      -- Blame follows from the role, Findler and Felleisen's negative and positive positions
    , ("an argument blames the caller",               blames (R.ArgumentOf (R.Defined "f") 1) R.Caller)
    , ("a result blames the function called",         blames (R.ResultOf (R.Defined "f")) (R.Called (R.Defined "f")))
    , ("an assertion blames itself",                  blames R.Assertion R.Itself)
    , ("as a question does",                          blames R.Asked R.Itself)

      -- The trail: each step the check took into the value, down to the part that failed
    , ("a value that fails as a whole has no trail",  failsWith "trail = []"
                                                              "(document g (:a <1 ⊢ string>))")
    , ("an array's element, counted from 1",          failsWith "stepPart = Element 2, stepType = Named RNumber"
                                                              "(define/procedure f (<xs ⊢ (array number)>) xs) (document g (:r (f (1 \"x\"))))")
    , ("a record's key",                              failsWith "stepPart = Key \"label\", stepType = Named RString"
                                                              "(define/procedure f (<r ⊢ (record :label string)>) r) (document g (:r (f (:label 1))))")
    , ("a key that is missing",                       failsWith "stepPart = Missing \"label\""
                                                              "(define/procedure f (<r ⊢ (record :label string)>) r) (document g (:r (f (:name \"a\"))))")
    , ("the operand of ∩ it is outside",              failsWith "stepPart = Whole, stepType = Named RString"
                                                              "(document g (:a <1 ⊢ (∩ string number)>))")
    , ("and a refinement's body it broke in",         failsWith "stepPart = Body \"broken\""
                                                              "(define/refinement broken (<x ⊢ string>) (doc \"b\") (eq <x ⊢ number> 1)) (document g (:r <\"s\" ⊢ broken>))")

      -- A defined refinement: its rule is its doc, and its domain is asked first
    , ("a defined refinement's rule is its doc",      failsWith "rule = Just \"has a title\""
                                                              (titled <> "(document g (:r <(:a 1) ⊢ titled>))"))
    , ("a value outside its domain fails as its argument", failsWith "role = ArgumentOf (Defined \"titled\") 1"
                                                              (titled <> "(document g (:r <1 ⊢ titled>))"))

      -- ∈ asks: outside, the answer is false; a refinement broken in its body is still an error
    , ("∈ is false outside a refinement",             compilesToJSON (titled <> "(document g (:r <(:a 1) ∈ titled>))")
                                                                   "{\"r\": false}")
    , ("and outside its domain",                      compilesToJSON (titled <> "(document g (:r <1 ∈ titled>))")
                                                                   "{\"r\": false}")
    , ("but a broken body is an error, in a union too", failsWith "stepPart = Body \"broken\""
                                                              "(define/refinement broken (<x ⊢ string>) (doc \"b\") (eq <x ⊢ number> 1)) (document g (:r <\"s\" ∈ (∪ broken number)>))")
    ]

    where
    titled = "(define/refinement titled (<r ⊢ record>) (doc \"has a title\") (has r title)) "

    blames role party = withTests 1 . property $ R.blame role === party


-- Every level of the built-in tree is a refinement that can be written: value, data and its
-- kinds, function, procedure and refinement. A function keeps its kind from its definition, a
-- built-in being a procedure. A value outside a built-in refinement is drawn in the tree, both
-- marked, unless the two are kinds of data side by side.
kinds :: Group
kinds = Group "Refinements: every level of the tree is written"
    [ ("each function is of its kind",                compilesToJSON (functions <> "(document g (:a1 <double ∈ procedure> :a2 <double ∈ refinement> :a3 <short ∈ refinement> :a4 <short ∈ procedure> :a5 <double ∈ function> :a6 <short ∈ function>))")
                                                                   "{\"a1\": true, \"a2\": false, \"a3\": true, \"a4\": false, \"a5\": true, \"a6\": true}")
    , ("a built-in and a fn are procedures",          compilesToJSON "(document g (:a1 <+ ∈ procedure> :a2 <+ ∈ refinement> :a3 <(fn (x) x) ∈ procedure>))"
                                                                   "{\"a1\": true, \"a2\": false, \"a3\": true}")
    , ("data is what can be written, value everything", compilesToJSON (functions <> "(document g (:a1 <1 ∈ data> :a2 <1 ∈ value> :a3 <double ∈ data> :a4 <double ∈ value> :a5 <\"s\" ∈ function>))")
                                                                   "{\"a1\": true, \"a2\": true, \"a3\": false, \"a4\": true, \"a5\": false}")
    , ("a function outside data fails",               failsWith "expectedRefinement = \"data\""
                                                              (functions <> "(document g (:a <double ⊢ data>))"))

      -- The tree in a message
    , ("the tree opens only the branches to a mark",  withTests 1 . property $
                                                      fmap (length . filter (not . T.null) . drop 1 . T.lines) (tree R.RProcedure number) === Just 10)
    , ("folding a branch with no mark below it",      withTests 1 . property $
                                                      fmap (any (T.isInfixOf "string …") . T.lines) (tree R.RProcedure number) === Just True)
    , ("a marked one too, when nothing below is",     withTests 1 . property $
                                                      fmap (any (T.isInfixOf "function …") . T.lines) (tree R.RFunction number) === Just True)
    , ("every built-in refinement is in the tree",    withTests 1 . property $
                                                      length R.builtInRefinements === 13)
    , ("marking the refinement required",             withTests 1 . property $
                                                      markedRow "◀ required" R.RProcedure number === Just "procedure")
    , ("and the kind given",                          withTests 1 . property $
                                                      markedRow "◀ given" R.RProcedure number === Just "number")
    , ("when the two are far apart",                  withTests 1 . property $
                                                      markedRow "◀ given" R.RUri number === Just "number")
    , ("but not when they are kinds of data side by side", withTests 1 . property $
                                                      tree R.RString number === Nothing)

      -- A refinement over value says nothing about its subject
    , ("a refinement over value is warned of",        warnsWith "The refinement 'anything' is declared over value"
                                                              "(define/refinement anything (<x ⊢ value>) (doc \"a\") true) (document g (:a <1 ∈ anything>))")
    , ("but not one over data",                       warnsWithout "declared over value"
                                                              "(define/refinement anything (<x ⊢ data>) (doc \"a\") true) (document g (:a <1 ∈ anything>))")
    ]

    where
    functions = "(define/procedure double (x) x) (define/refinement short (<x ⊢ string>) (doc \"r\") true) "

    number = Primitive (I.Number 1)

    tree = R.refinementTree

    -- The name on the row that carries the mark, the tree's branches trimmed from it
    markedRow :: Text -> R.Refinement -> Expr -> Maybe Text
    markedRow mark required value = do
        drawn <- tree required value
        row   <- case filter (mark `T.isSuffixOf`) (T.lines drawn) of
                     [only] -> Just only
                     _rows  -> Nothing
        case T.words (T.filter (`notElem` ("├└│─" :: String)) row) of
            name : _ -> Just name
            []       -> Nothing
