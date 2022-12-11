module Day02
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Helpers
import Parsers

data RPS  = Rock | Paper | Scissors deriving (Eq, Show)
data WLD  = Win | Lose | Draw deriving (Eq, Show)

type Parsed = [(Char, Char)]

charToRPS :: Char -> RPS
charToRPS 'A' = Rock
charToRPS 'B' = Paper
charToRPS 'C' = Scissors
charToRPS 'X' = Rock
charToRPS 'Y' = Paper
charToRPS 'Z' = Scissors
charToRPS c   = error $ show c

charToWLD :: Char -> WLD
charToWLD 'X' = Lose
charToWLD 'Y' = Draw
charToWLD 'Z' = Win
charToWLD c   = error $ show c

parseLine :: Parser (Char, Char)
parseLine = do
  char1 <- P.anyChar
  P.space
  char2 <- P.anyChar
  P.newline
  return (char1, char2)

parse :: Parser Parsed
parse = P.many1 parseLine

roundScore :: (RPS, RPS) -> Int
roundScore (o, me) = wintielose + selection
  where selection   | me == Rock                    = 1
                    | me == Paper                   = 2
                    | me == Scissors                = 3
                    | otherwise                     = 0
        wintielose  | o == me                       = 3
                    | o == Rock && me == Paper      = 6
                    | o == Paper && me == Scissors  = 6
                    | o == Scissors && me == Rock   = 6
                    | otherwise                     = 0

part1 :: Parsed -> Int
part1 parsed = sum $ map (roundScore . convert) parsed
  where convert (c1, c2) = (charToRPS c1, charToRPS c2)

strategy :: RPS -> WLD -> RPS
strategy rps Draw       = rps
strategy Rock Lose      = Scissors
strategy Rock Win       = Paper
strategy Paper Lose     = Rock
strategy Paper Win      = Scissors
strategy Scissors Lose  = Paper
strategy Scissors Win   = Rock

part2 :: Parsed -> Int
part2 parsed = sum $ map (roundScore . convert) parsed
  where convert (c1, c2)  = (charToRPS c1, strategy (charToRPS c1) (charToWLD c2))

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  print $ part1 parsed
  print $ part2 parsed
