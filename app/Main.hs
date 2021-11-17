module Main (main) where

import Day1
import Text.Printf (printf)

days :: [IO ()]
days = [day1]

main :: IO ()
main = mapM_ runDay $ zip [1 ..] days
  where
    runDay :: (Int, IO ()) -> IO ()
    runDay (dayNum, day) = printf "Day %d:\n" dayNum >> day
