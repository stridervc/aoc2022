module Day04
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Helpers
import Parsers

type Assignment = (Int, Int)
type Pair       = (Assignment, Assignment)

type Parsed = [Pair]

parseAssignment :: Parser Assignment
parseAssignment = do
  i1 <- parseInt
  P.char '-'
  i2 <- parseInt
  return (i1, i2)

parseLine :: Parser Pair
parseLine = do
  a1 <- parseAssignment
  P.char ','
  a2 <- parseAssignment
  P.newline
  return (a1, a2)

parse :: Parser Parsed
parse = P.many parseLine

fullyContained :: Assignment -> Assignment -> Bool
fullyContained (b1, e1) (b2, e2)
  | b2 >= b1 && e2 <= e1  = True
  | b1 >= b2 && e1 <= e2  = True
  | otherwise             = False

overlaps :: Assignment -> Assignment -> Bool
overlaps (b1, e1) (b2, e2)
  | b2 >= b1 && b2 <= e1  = True
  | b1 >= b2 && b1 <= e2  = True
  | e1 >= b2 && e1 <= e2  = True
  | e2 >= b1 && e2 <= e1  = True
  | otherwise             = False

part1 :: Parsed -> Int
part1 parsed = length $ filter id $ map (uncurry fullyContained) parsed

part2 :: Parsed -> Int
part2 parsed = length $ filter id $ map (uncurry overlaps) parsed

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  print $ part1 parsed
  print $ part2 parsed
