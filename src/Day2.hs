{-# LANGUAGE RecordWildCards #-}

module Day2 (day2) where

import Common
import Data.Function ((&))
import Data.Maybe (fromMaybe, isNothing)
import GHC.Ix (inRange)
import Text.Megaparsec
import Text.Megaparsec.Char

data Policy = Policy
  { passMin :: Int,
    passMax :: Int,
    passChar :: Char,
    password :: String
  }
  deriving (Show)

parser :: Parser [Policy]
parser = many parsePolicy
  where
    parsePolicy :: Parser Policy
    parsePolicy = Policy <$> parseNum <*> parseNum <*> parseChar <*> parsePassword
    parseNum = read <$> (some digitChar <* anySingle)
    parseChar = lowerChar <* string ": "
    parsePassword = some lowerChar <* many spaceChar

part1 :: Policy -> Bool
part1 Policy {..} = password & filter (== passChar) & length & inRange (passMin, passMax)

part2 :: Policy -> Bool
part2 Policy {..} = check passMin /= check passMax
  where
    check nth = (password !! (nth - 1)) == passChar

testWith :: (Policy -> Bool) -> [Policy] -> Int
testWith isValidPolicy = length . filter isValidPolicy

day2 :: Day
day2 = Day 2 parser (testWith part1) (testWith part2)

testInput =
  "1-3 a: abcde\n\
  \1-3 b: cdefg\n\
  \2-9 c: ccccccccc"
