module Domain.Validation (
  validate,
  rangeBetween,
  lengthBetween,
  regexMatches) where

import Prelude hiding (length)
import Data.Text
import Data.Maybe
import Text.Regex.PCRE.Heavy
import Control.Lens

-- https://edu.anarcho-copy.org/Programming%20Languages/Haskell/Practical%20Web%20Development%20with%20Haskell.pdf
-- ebook p58

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate onValid validators val =
  case foldMapOf folded (\validator -> maybeToList $ validator val) validators of
    [] -> Right $ onValid val
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween min max err val =
  if val >= min && val <= max then Nothing else Just err

lengthBetween :: Int -> Int -> e -> Validation e Text
lengthBetween min max err val = rangeBetween min max err (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex err val = if val =~ regex then Nothing else Just err