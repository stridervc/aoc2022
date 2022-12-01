module Day02
  ( solve
  ) where

type Parsed = String

parse :: String -> Parsed
parse input = input

part1 :: Parsed -> String
part1 parsed = "Not yet implemented"

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where parsed  = parse input
