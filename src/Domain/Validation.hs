module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy
import Control.Lens

-- p58

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate onValid validators val =
  case foldMapOf (\validator -> maybeToList $ validator val) validators of
    [] -> Right $ onValid val
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween min max err val =
  if val >= min && val <= max then Nothing else Just err

lengthBetween :: (Ord a) => a -> a -> e -> Validation e a
lengthBetween min max err val = rangeBetween min max err (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex err val = if regex =~ val then Nothing else Just err