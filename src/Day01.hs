module Day01
  ( solve
  ) where

import Data.List (sort)
import Data.List.Split (splitOn)

type Parsed = [[Int]]

parse :: String -> Parsed
parse input = map (map read) $ splitOn [""] $ lines input

part1 :: Parsed -> Int
part1 = maximum . map sum

part2 :: Parsed -> Int
part2 inputs = sum $ take 3 $ reverse $ sort $ map sum inputs

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where parsed  = parse input
