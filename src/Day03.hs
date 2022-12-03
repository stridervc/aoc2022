module Day03
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers

import Data.Char (isLower, ord)
import Data.List (elem, nub)

type Parsed = [String]

parseLine :: Parser String
parseLine = P.manyTill P.anyChar P.newline

parse :: Parser Parsed
parse = P.many parseLine

splitCompartments :: String -> (String, String)
splitCompartments is = splitAt (n `div` 2) is
  where n = length is

charToPriority :: Char -> Int
charToPriority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

findCommon :: Eq a => ([a], [a]) -> [a]
findCommon ([], _)        = []
findCommon (_, [])        = []
findCommon (l:ls, rs)
  | l `elem` rs = nub $ l : findCommon (ls,rs)
  | otherwise   = findCommon (ls,rs)

part1 :: Parsed -> Int
part1 parsed = sum $ map charToPriority $ concatMap (findCommon . splitCompartments) parsed

type Group = (String, String, String)

groups :: [String] -> [Group]
groups [] = []
groups is = (head is, is!!1, is!!2) : groups (drop 3 is)

commonInGroup :: Group -> String
commonInGroup ([], _, _)  = []
commonInGroup (s1, s2, s3)
  | head s1 `elem` s2 && head s1 `elem` s3  = nub $ head s1 : commonInGroup (tail s1, s2, s3)
  | otherwise                               = commonInGroup (tail s1, s2, s3)

part2 :: Parsed -> Int
part2 parsed = sum $ map charToPriority $ concatMap commonInGroup $ groups parsed

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
