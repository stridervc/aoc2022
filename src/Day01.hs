module Day01
  ( solve
  ) where

import Data.List (sort)
import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse input = map (map read) $ splitOn [""] $ lines input

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 inputs = sum $ take 3 $ reverse $ sort $ map sum inputs

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where parsed  = parse input
