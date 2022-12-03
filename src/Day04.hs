module Day04
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers

type Parsed = String

parse :: Parser Parsed
parse = P.many P.anyChar

part1 :: Parsed -> String
part1 parsed = "Not yet implemented"

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve input = do
  print $ P.parse parse "(input)" input
  {-
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
  -}
