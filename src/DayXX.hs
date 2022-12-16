module DayXX
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers
import Helpers

type Parsed = String

parse :: Parser Parsed
parse = P.many P.anyChar

part1 :: Parsed -> String
part1 parsed = "Not yet implemented"

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile False day parse
  print $ part1 parsed
  print $ part2 parsed
