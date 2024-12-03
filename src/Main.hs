module Main where

import D1 (day1)
import D2 (day2)
import D3 (day3)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    [day, fileName] -> case day of
      "1" -> day1 fileName
      "2" -> day2 fileName
      "3" -> day3 fileName
      _   -> error "Invalid day number provided."
    _ -> error "Usage: <program> <day> <fileName>"
