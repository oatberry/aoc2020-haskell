module Day3 (day3) where

import Common
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type TreeMap = Vector (Vector Int)

parseInput :: String -> TreeMap
parseInput = V.fromList . fmap (V.fromList . fmap treeToInt) . lines
  where
    treeToInt '.' = 0
    treeToInt '#' = 1

toboggan :: (Int, Int) -> TreeMap -> Int
toboggan (dx, dy) treeMap = go (0, 0) 0
  where
    go (x, y) n = case treeMap !? y of
      Nothing -> n
      Just row ->
        go
          (x + dx, y + dy)
          (n + (row ! (x `mod` length row)))

part1 :: String -> String
part1 = show . toboggan (3, 1) . parseInput

part2 :: String -> String
part2 = show . product . flip map slopes . flip toboggan . parseInput
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

day3 :: Day
day3 = Day 3 [part1, part2]
