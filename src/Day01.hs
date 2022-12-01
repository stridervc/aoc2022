module Day01
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers

import Data.List (sort)

type Parsed = [[Int]]

-- | parse newline terminated rows of Ints until we can't
parseInts :: Parser [Int]
parseInts = do
  P.many1 $ do
    num <- parseInt
    P.newline
    return num

-- | groups are separated by newlines (or EOF)
parse :: Parser Parsed
parse = P.sepBy parseInts P.newline

part1 :: Parsed -> Int
part1 = maximum . map sum

part2 :: Parsed -> Int
part2 inputs = sum $ take 3 $ reverse $ sort $ map sum inputs

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
