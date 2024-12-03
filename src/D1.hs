module D1 where

import Paths_aoc2024 (getDataFileName)
import Data.List

day1 :: IO ()
day1 = do
    content <- getDataFileName "1.txt" >>= readFile
    let parsedInput = parseInput content
    putStrLn "Part 1:"
    print (solvePart1 parsedInput)
    putStrLn "Part 2:"
    print (solvePart2 parsedInput)

parseInput :: String -> ([Int], [Int])
parseInput input = (firstColumn, secondColumn)
  where
    linesInput = lines input
    firstColumn = map (read . head . words) linesInput
    secondColumn = map (read . last . words) linesInput

solvePart1 :: ([Int], [Int]) -> Int
solvePart1 (lst1, lst2) = sum $ zipWith (\x y -> abs (x - y)) (sort lst1) (sort lst2)

-- We could use memoization/state here but that sounds like too much work
solvePart2 :: InputType -> OutputType
solvePart2 (lst1, lst2) = sum $ map (\x -> x * occurrences x lst2) lst1
  where
    occurrences x = length . filter (== x)

type InputType = ([Int], [Int])
type OutputType = Int

