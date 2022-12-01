module Day01
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (sort)

import Parsers

type Parsed = [[Int]]

-- | parse newline terminated rows of Ints until we can't
parseGroup :: Parser [Int]
parseGroup = do
  P.many1 $ do
    num <- parseInt
    P.newline
    return num

-- | groups are separated by newlines (or EOF)
parseAll :: Parser Parsed
parseAll = P.sepBy parseGroup P.newline

parse :: String -> Parsed
parse input = do
  case P.parse parseAll "(input)" input of
    Left e  -> error $ show e
    Right p -> p

part1 :: Parsed -> Int
part1 = maximum . map sum

part2 :: Parsed -> Int
part2 inputs = sum $ take 3 $ reverse $ sort $ map sum inputs

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where parsed  = parse input
