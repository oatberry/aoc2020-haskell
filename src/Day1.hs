{-# LANGUAGE BlockArguments #-}

module Day1 (day1) where

import Common
import Control.Monad (guard)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

parse :: String -> ([Int], IntSet)
parse rawInput =
  let input = map read $ lines rawInput
   in (input, IntSet.fromList input)

part1 :: String -> String
part1 rawInput =
  let (nums, numSet) = parse rawInput
   in firstOrNothing do
        x <- nums
        let y = 2020 - x
        guard $ IntSet.member y numSet
        pure $ x * y

part2 :: String -> String
part2 rawInput =
  let (nums, numSet) = parse rawInput
   in firstOrNothing do
        x <- nums
        y <- nums
        let z = 2020 - x - y
        guard $ IntSet.member z numSet
        pure $ x * y * z

day1 :: Day
day1 = Day 1 [part1, part2]

testInput1 =
  "1721\n\
  \979\n\
  \366\n\
  \299\n\
  \675\n\
  \1456\n"
