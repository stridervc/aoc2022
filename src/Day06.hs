module Day06
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (nub)

import Parsers

type Parsed = String

parse :: Parser Parsed
parse = P.many P.anyChar

distinct :: String -> Bool
distinct str = length str == length (nub str)

subroutine :: Int -> Int -> String -> Int
subroutine n count str
  | distinct (take n str) = count + n
  | otherwise             = subroutine n (count+1) (tail str)

part1 :: Parsed -> Int
part1 = subroutine 4 0

part2 :: Parsed -> Int
part2 = subroutine 14 0

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
