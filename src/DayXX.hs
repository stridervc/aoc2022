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

part1 :: Parsed -> IO ()
part1 parsed = do
  putStrLn "Not yet implemented"

part2 :: Parsed -> IO ()
part2 parsed = do
  putStrLn "Not yet implemented"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile False day parse
  part1 parsed
  part2 parsed
