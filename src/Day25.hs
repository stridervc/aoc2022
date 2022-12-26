module Day25
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers
import Helpers

type Snafu  = String
type Parsed = [Snafu]

parseLine :: Parser Snafu
parseLine = P.manyTill P.anyChar P.newline

parse :: Parser Parsed
parse = P.many parseLine

snafuCharToInt :: Int -> Char -> Int
snafuCharToInt place '='  = -2 * (5^place)
snafuCharToInt place '-'  = -1 * (5^place)
snafuCharToInt place '0'  = 0
snafuCharToInt place '1'  = 1 * (5^place)
snafuCharToInt place '2'  = 2 * (5^place)
snafuCharToInt _     _    = error "Unexpected snafu char"

snafuToInt :: Snafu -> Int
snafuToInt snafu = sum $ zipWith snafuCharToInt [0..] $ reverse snafu

digitToSnafu :: Int -> Char
digitToSnafu (-2) = '='
digitToSnafu (-1) = '-'
digitToSnafu 0    = '0'
digitToSnafu 1    = '1'
digitToSnafu 2    = '2'
digitToSnafu _    = error "Unexpeted digit"

intToSnafu :: Int -> Snafu
intToSnafu 0    = ""
intToSnafu num  = intToSnafu ((num+2) `div` 5) <> [ digitToSnafu ((num+2) `mod` 5 - 2) ]

part1 :: Parsed -> IO ()
part1 parsed = do
  putStrLn $ intToSnafu $ sum $ map snafuToInt parsed

part2 :: Parsed -> IO ()
part2 parsed = do
  putStrLn "No part 2"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  part1 parsed
  part2 parsed
