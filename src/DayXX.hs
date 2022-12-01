module DayXX
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers

type Parsed = String

-- | Do nothing useful for now
parseAll :: Parser Parsed
parseAll = P.many P.anyChar

parse :: String -> Parsed
parse input = do
  case P.parse parseAll "(input)" input of
    Left e  -> error $ show e
    Right p -> p

part1 :: Parsed -> String
part1 parsed = "Not yet implemented"

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where parsed  = parse input
