module Day05
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (transpose)
import Data.Maybe (catMaybes)

import qualified Data.IntMap as IM

import Parsers

type Crate    = Char
type Stack    = [Crate]           -- left of list is top of stack
type Stacks   = IM.IntMap Stack   -- convenient replacing of a stack and can start index at 1
type Count    = Int
type StackNo  = Int
type Rule     = (Count, StackNo, StackNo)

type Parsed = (Stacks, [Rule])

parseMaybeCrate :: Parser (Maybe Crate)
parseMaybeCrate = P.choice
  [ do
    P.char '['
    c <- P.anyChar
    P.char ']'
    return $ Just c
  , do
    P.count 3 P.space
    return Nothing
  ]

parseCrateRow :: Parser [Maybe Crate]
parseCrateRow = do
  mcs <- P.sepBy parseMaybeCrate (P.char ' ')
  P.newline
  return mcs

-- | A line that starts with space digit space, followed by anything else
-- | We don't care about the result
parseStackNumbers :: Parser ()
parseStackNumbers = do
  P.char ' '
  P.digit
  P.char ' '
  P.manyTill P.anyChar P.newline
  return ()

parseStacks :: Parser Stacks
parseStacks = do
  rows <- P.manyTill parseCrateRow (P.try parseStackNumbers)
  return $ IM.fromList $ zip [1..] $ map catMaybes $ transpose rows

parseRule :: Parser Rule
parseRule = do
  P.string "move "
  count <- parseInt
  P.string " from "
  from <- parseInt
  P.string " to "
  to <- parseInt
  P.newline
  return (count, from, to)

parse :: Parser Parsed
parse = do
  stacks <- parseStacks
  P.newline
  rules <- P.many parseRule
  return (stacks, rules)

applyRule :: Rule -> Stacks -> Stacks
applyRule (count, from, to) stacks
  | count == 0  = stacks
  | otherwise   = applyRule (count-1, from, to) stacks'
  where fromStack   = IM.findWithDefault [] from stacks
        toStack     = IM.findWithDefault [] to stacks
        crate       = head fromStack
        fromStack'  = tail fromStack
        toStack'    = crate : toStack
        stacks'     = IM.insert from fromStack' $ IM.insert to toStack' stacks

applyRule2 :: Rule -> Stacks -> Stacks
applyRule2 (count, from, to) stacks = IM.insert from fromStack' $ IM.insert to toStack' stacks
  where fromStack   = IM.findWithDefault [] from stacks
        toStack     = IM.findWithDefault [] to stacks
        crates      = take count fromStack
        fromStack'  = drop count fromStack
        toStack'    = crates <> toStack

part1 :: Parsed -> String
part1 (stacks, rules) = map (head . snd) $ IM.toList stacks'
  where stacks' = foldl (flip applyRule) stacks rules

part2 :: Parsed -> String
part2 (stacks, rules) = map (head . snd) $ IM.toList stacks'
  where stacks' = foldl (flip applyRule2) stacks rules

solve :: String -> IO ()
solve input = do
  putStrLn $ part1 parsed
  putStrLn $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
