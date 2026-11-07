
module Data.Ouro.Internal.Utils where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Text     (Text)
import qualified Data.Text     as T


-- Levenshtein distance calculation.
-- Measures the minimum edit operations to turn the first Text into the second.
levenshtein :: Text -> Text -> Int
levenshtein s1 s2 = last $ foldl' transform [0 .. T.length s2] (T.unpack s1)
    where
    transform dists@(d:ds) c = scanl (\left (above, aboveLeft, char2)
                                         -> minimum [left + 1, above + 1, aboveLeft + substitution c char2]
                                     ) (d + 1) (zip3 ds dists (T.unpack s2))
    transform [] _ = []

    -- Changing a character costs one; keeping it costs nothing
    substitution a b = case a == b of
                           True  -> 0
                           False -> 1


-- Ranks a list of candidate strings by similarity to a target string.
rankBySimilarity :: Text -> [Text] -> [(Text, Int)]
rankBySimilarity target candidates =
    let unrankedPairs = map (\cand -> (cand, levenshtein target cand)) candidates
    in sortBy (compare `on` snd) unrankedPairs
