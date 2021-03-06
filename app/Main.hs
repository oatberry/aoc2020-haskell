module Main (main) where

import Common
import Day1
import Day2
import Day3

days :: [Day]
days = [day1, day2, day3]

main :: IO ()
main = mapM_ runDay days
