{-# LANGUAGE BlockArguments #-}

module Day1 (day1) where

import Common
import Control.Monad (guard)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

parser :: String -> ([Int], IntSet)
parser rawInput =
  let input = map read $ lines rawInput
   in (input, IntSet.fromList input)

part1 :: ([Int], IntSet) -> Maybe Int
part1 (nums, numSet) =
  listToMaybe do
    x <- nums
    let y = 2020 - x
    guard $ IntSet.member y numSet
    pure $ x * y

part2 :: ([Int], IntSet) -> Maybe Int
part2 (nums, numSet) =
  listToMaybe do
    x <- nums
    y <- nums
    let z = 2020 - x - y
    guard $ IntSet.member z numSet
    pure $ x * y * z

day1 :: Day
day1 = Day 1 (simpleParser parser) part1 part2

testInput1 =
  "1721\n\
  \979\n\
  \366\n\
  \299\n\
  \675\n\
  \1456\n"
