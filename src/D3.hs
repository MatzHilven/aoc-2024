module D3 where

import Paths_aoc2024 (getDataFileName)
import Text.Regex.TDFA
import Data.List

day3 :: String -> IO ()
day3 fileName = do
    content <- getDataFileName fileName >>= readFile
    let parsedInput = parseInput content
    print parsedInput
    putStrLn "Part 1:"
    print (solvePart1 parsedInput)
    putStrLn "Part 2:"
    print (solvePart2 parsedInput)

parseInput :: String -> InputType
parseInput input = getAllTextMatches (input =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)")

extractNumbers :: String -> (Int, Int)
extractNumbers str
  | "mul(" `isPrefixOf` str = 
      let numbers = getAllTextMatches (str =~ "[0-9]{1,3}") :: [String]
      in (read (head numbers), read (numbers !! 1))
  | "do()" `isInfixOf` str || "don't()" `isInfixOf` str = 
      (0, 0)
  | otherwise = 
      (0, 0)


solvePart1 :: InputType -> OutputType
solvePart1 = foldr ((\(x,y) acc -> acc + (x*y)) . extractNumbers) 0

solvePart2 :: InputType -> OutputType
solvePart2 input = go input True 0
  where
    go :: [String] -> Bool -> OutputType -> OutputType
    go [] _ acc                          = acc
    go ("do()" : rest) _ acc             = go rest True acc
    go ("don't()" : rest) _ acc          = go rest False acc
    
    go (m : xs) mulEnabled acc     
      | mulEnabled = 
          let (x, y) = extractNumbers m
          in go xs mulEnabled (acc + (x * y))
      | otherwise  = go xs mulEnabled acc

    
    


type InputType = [String]
type OutputType = Int

