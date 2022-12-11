module Day11
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import qualified Data.Map.Strict as M

import Data.List (sort)

import Parsers

type Item           = Integer
type MonkeyID       = Int
data Operation      = Mult Int | Plus Int | Sqr deriving (Eq, Show)
type InspectCounts  = M.Map MonkeyID Integer

data Monkey = Monkey
  { monkeyID        :: MonkeyID   -- ^ not used, id = index in list
  , monkeyItems     :: [Item]
  , monkeyOperation :: Operation
  , monkeyDivide    :: Int
  , monkeyTrue      :: Int
  , monkeyFalse     :: Int
  } deriving (Eq, Show)

type Parsed     = [Monkey]
type GameState  = (InspectCounts, [Monkey])

parseMonkey :: Parser Monkey
parseMonkey = do
  P.string "Monkey "
  id <- parseInt
  P.char ':' >> P.newline
  P.spaces >> P.string "Starting items: "
  items <- P.sepBy parseInt (P.string ", ")
  P.newline
  P.spaces >> P.string "Operation: new = old "
  op <- P.anyChar
  P.space
  opval <- P.string "old" <|> P.many1 P.digit
  P.newline
  P.spaces >> P.string "Test: divisible by "
  divby <- parseInt
  P.newline
  P.spaces >> P.string "If true: throw to monkey "
  true <- parseInt
  P.newline
  P.spaces >> P.string "If false: throw to monkey "
  false <- parseInt
  P.newline

  let operation | opval == "old"  = Sqr
                | otherwise       = if op == '*' then Mult (read opval) else Plus (read opval)

  return $ Monkey
    { monkeyID        = id
    , monkeyItems     = items
    , monkeyOperation = operation
    , monkeyDivide    = divby
    , monkeyTrue      = true
    , monkeyFalse     = false
    }

parse :: Parser Parsed
parse = P.sepBy parseMonkey P.newline

applyOp :: Operation -> Item -> Item
applyOp (Mult i)  item  = item * fromIntegral i
applyOp (Plus i)  item  = item + fromIntegral i
applyOp Sqr       item  = item * item

inspectItems :: Monkey -> Monkey
inspectItems m = m { monkeyItems = items' }
  where op        = monkeyOperation m
        items'    = map (\i -> applyOp op i `div` 3) (monkeyItems m)

inspectItems2 :: Monkey -> Monkey
inspectItems2 m = m { monkeyItems = items' }
  where op        = monkeyOperation m
        items'    = map (applyOp op) (monkeyItems m)

updateMonkey :: MonkeyID -> Monkey -> [Monkey] -> [Monkey]
updateMonkey id m ms  = take id ms <> [m] <> drop (id+1) ms

throwItem :: MonkeyID -> MonkeyID -> [Monkey] -> [Monkey]
throwItem from to ms = updateMonkey from fromM' $ updateMonkey to toM' ms
  where fromM                 = ms!!from
        fromM'                = fromM { monkeyItems = tail (monkeyItems fromM) }
        toM                   = ms!!to
        toM'                  = toM { monkeyItems = monkeyItems toM <> [ head (monkeyItems fromM) ] }

monkeyTurn :: MonkeyID -> GameState -> GameState
monkeyTurn mi (counts,ms)
  | mi >= length ms = (counts, ms)
  | otherwise       = (counts', foldl (flip (throwItem mi)) ms' dests)
  where m       = inspectItems (ms!!mi)
        ms'     = updateMonkey mi m ms
        divby   = monkeyDivide m
        tests   = map (\i -> i `mod` fromIntegral divby == 0) (monkeyItems m)
        dests   = map (\tf -> if tf then monkeyTrue m else monkeyFalse m) tests
        count   = M.findWithDefault 0 mi counts
        counts' = M.insert mi (count+fromIntegral (length (monkeyItems m))) counts

monkeyTurn2 :: MonkeyID -> GameState -> GameState
monkeyTurn2 mi (counts,ms)
  | mi >= length ms = (counts, ms)
  | otherwise       = (counts', foldl (flip (throwItem mi)) ms' dests)
  where m       = inspectItems2 (ms!!mi)
        ms'     = updateMonkey mi m ms
        divby   = monkeyDivide m
        tests   = map (\i -> i `mod` fromIntegral divby == 0) (monkeyItems m)
        dests   = map (\tf -> if tf then monkeyTrue m else monkeyFalse m) tests
        count   = M.findWithDefault 0 mi counts
        counts' = M.insert mi (count+fromIntegral (length (monkeyItems m))) counts

monkeyRound :: GameState -> GameState
monkeyRound state@(_,ms)  = foldl (flip monkeyTurn) state [0..length ms - 1]

monkeyRound2 :: GameState -> GameState
monkeyRound2 state@(_,ms)  = foldl (flip monkeyTurn2) state [0..length ms - 1]

part1 :: Parsed -> Integer
part1 ms = head counts * counts!!1
  where rounds  = foldl (\state i -> monkeyRound state) (mempty, ms) [1..20]
        counts  = reverse $ sort $ map snd $ M.toList $ fst rounds

-- 14399879980 too low
part2 :: Parsed -> Integer
part2 ms = head counts * counts!!1
  where rounds  = foldl (\state i -> monkeyRound2 state) (mempty, ms) [1..10000]
        counts  = reverse $ sort $ map snd $ M.toList $ fst rounds

solve :: String -> IO ()
solve input = do
  -- print $ P.parse parse "(input)" input
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
