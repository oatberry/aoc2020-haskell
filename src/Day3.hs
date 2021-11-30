module Day3 (day3) where

import Common
import Data.Functor ((<&>))
import Data.List (unfoldr)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type TreeMap = Vector (Vector Int)

parser :: String -> TreeMap
parser = V.fromList . fmap (V.fromList . fmap treeToInt) . lines
  where
    treeToInt '.' = 0
    treeToInt '#' = 1

toboggan :: (Int, Int) -> TreeMap -> Int
toboggan (dx, dy) treeMap = sum $ unfoldr next (0, 0)
  where
    -- I'm sorry, I couldn't resist
    next (x, y) = treeMap !? y <&> \row -> (row ! (x `mod` length row), (x + dx, y + dy))

part1 :: TreeMap -> Int
part1 = toboggan (3, 1)

part2 :: TreeMap -> Int
part2 = product . flip map slopes . flip toboggan
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

day3 :: Day
day3 = Day 3 (simpleParser parser) part1 part2
