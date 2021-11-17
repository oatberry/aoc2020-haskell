{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( getAOCInput,
    runParts,
    firstOrNothing,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (listToMaybe)
import Network.HTTP.Req
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

getCookie :: IO C.ByteString
getCookie = do
  perhapsCookie <- try $ C.readFile "./inputs/cookie"
  case perhapsCookie of
    Left (_ :: IOException) -> failCookie
    Right "" -> failCookie
    Right cookie -> pure cookie
  where
    failCookie = do
      hPutStrLn stderr "Error: Please put your AoC session cookie into ./inputs/cookie"
      exitFailure

fetchInput :: Int -> IO String
fetchInput dayNum = do
  printf "Downloading input for day %d...\n" dayNum
  cookie <- getCookie
  let url = https "adventofcode.com" /: "2020" /: "day" /~ dayNum /: "input"
      headers = header "Cookie" cookie
      request = req GET url NoReqBody bsResponse headers
  response <- runReq defaultHttpConfig request
  let input = C.unpack . responseBody $ response
  writeFile ("./inputs/day" ++ show dayNum) input
  pure input

getAOCInput :: Int -> IO String
getAOCInput dayNum = do
  let inputFile = "./inputs/day" ++ show dayNum
  perhapsInput <- try $ readFile inputFile
  case perhapsInput of
    Left (_ :: IOException) -> fetchInput dayNum
    Right input -> pure input

runParts :: [String -> String] -> String -> IO ()
runParts parts input = mapM_ runPart $ zip [1 ..] parts
  where
    runPart :: (Int, String -> String) -> IO ()
    runPart (partNum, part) = printf "  part %d: %s\n" partNum $ part input

firstOrNothing :: Show a => [a] -> String
firstOrNothing = maybe "Nothing" show . listToMaybe
