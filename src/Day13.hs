module Day13
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (sort)

import Parsers

data PacketData = PacketInt Int | PacketList [PacketData] deriving (Eq, Show)

instance Ord PacketData where
  compare (PacketInt a) (PacketInt b)   = compare a b
  compare (PacketList a) (PacketList b) = compare a b
  compare (PacketInt a) (PacketList b)  = compare (PacketList [PacketInt a]) (PacketList b)
  compare (PacketList a) (PacketInt b)  = compare (PacketList a) (PacketList [PacketInt b])

type PacketPair = (PacketData, PacketData)
type Parsed     = [PacketPair]

parsePacketInt :: Parser PacketData
parsePacketInt = PacketInt <$> parseInt

parsePacketList :: Parser PacketData
parsePacketList = do
  P.char '['
  dat <- P.sepBy (parsePacketList <|> parsePacketInt) (P.char ',')
  P.char ']'
  return $ PacketList dat

parsePacketPair :: Parser PacketPair
parsePacketPair = do
  left <- parsePacketList
  P.newline
  right <- parsePacketList
  P.newline
  return (left, right)

parse :: Parser Parsed
parse = P.sepBy parsePacketPair P.newline

part1 :: Parsed -> Int
part1 pairs = sum [ i | (i, correct) <- zip [1..] (map correctOrder pairs), correct ]
  where correctOrder  = uncurry (<)

part2 :: Parsed -> Int
part2 pairs = index1 * index2
  where divider1  = PacketList [PacketList [PacketInt 2]]
        divider2  = PacketList [PacketList [PacketInt 6]]
        packets   = sort $ divider1 : divider2 : concat [ [l, r] | (l, r) <- pairs ]
        index1    = length (takeWhile (/= divider1) packets) + 1
        index2    = length (takeWhile (/= divider2) packets) + 1

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  print $ part1 parsed
  print $ part2 parsed
