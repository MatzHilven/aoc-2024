module D2 where

import Paths_aoc2024 (getDataFileName)

day2 :: IO ()
day2 = do
    content <- getDataFileName "2.txt" >>= readFile
    let parsedInput = parseInput content
    print parsedInput
    putStrLn "Part 1:"
    print (solvePart1 parsedInput)
    putStrLn "Part 2:"
    print (solvePart2 parsedInput)

parseInput :: String -> InputType
parseInput input = map (map read . words) (lines input)

mapWithNext :: (a -> a -> b) -> [a] -> [b]
mapWithNext _ []       = []
mapWithNext _ [_]      = []
mapWithNext f (x:y:xs) = f x y : mapWithNext f (y:xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe ys = all validDiff diffs && consistentDirection diffs
  where
    diffs = mapWithNext (-) ys
    validDiff d = abs d > 0 && abs d <= 3
    consistentDirection ds = all (> 0) ds || all (< 0) ds

solvePart1 :: InputType -> OutputType
solvePart1 = length . filter isSafe

solvePart2 :: InputType -> OutputType
solvePart2 = length . filter go
  where
    go :: [Int] -> Bool
    go xs
      | isSafe xs = True
      | otherwise = any (isSafe . removeAt xs) [0 .. length xs - 1]

    removeAt :: [a] -> Int -> [a]
    removeAt ys n = take n ys ++ drop (n + 1) ys    

type InputType = [[Int]]
type OutputType = Int

