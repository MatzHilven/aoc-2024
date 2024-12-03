module Main where

import D1 (day1)
import D2 (day2)
import D3 (day3)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : _ -> day1
    "2" : _ -> day2
    "3" : _ -> day3
    _ -> error "None or invalid day number provided."
