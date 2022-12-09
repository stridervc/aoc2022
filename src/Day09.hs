module Day09
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (nub)
import qualified Data.Set as S

import Parsers

type Coord  = (Int, Int)
type Rope   = (Coord, Coord)    -- ^ (Head, Tail)
type Motion = (Char, Int)
type Parsed = [Motion]

type TailCoords = S.Set Coord
type RopeSet    = (Rope, TailCoords)

startRope :: Rope
startRope = ((0, 0), (0, 0))

startRopeSet :: RopeSet
startRopeSet = (startRope, S.singleton (0, 0))

{-
touching :: Rope -> Bool
touching (h, t) = dx <= 1 && dy <= 1
  where dx  = abs $ fst h - fst t
        dy  = abs $ snd h - snd t
-}

catchup :: RopeSet -> RopeSet
catchup (r@(h, t), set)
  | (abs dx <= 1) && (abs dy <= 1)  = (r, set)
  | otherwise                       = ((h, (tx+cx, ty+cy)), S.insert (tx+cx, ty+cy) set)
  where hx  = fst h
        hy  = snd h
        tx  = fst t
        ty  = snd t
        dx  = hx - tx
        dy  = hy - ty
        cy  | dy > 0    = 1
            | dy < 0    = -1
            | otherwise = 0
        cx  | dx > 0    = 1
            | dx < 0    = -1
            | otherwise = 0

move :: Motion -> RopeSet -> RopeSet
move (_, 0) r               = r
move ('U', c) (((hx, hy), t), set) = move ('U', c-1) $ catchup (((hx, hy-1), t), set)
move ('D', c) (((hx, hy), t), set) = move ('D', c-1) $ catchup (((hx, hy+1), t), set)
move ('L', c) (((hx, hy), t), set) = move ('L', c-1) $ catchup (((hx-1, hy), t), set)
move ('R', c) (((hx, hy), t), set) = move ('R', c-1) $ catchup (((hx+1, hy), t), set)
move _ _                    = error "Unexpected motion in move"

parseMotion :: Parser Motion
parseMotion = do
  dir <- P.anyChar
  P.space
  count <- parseInt
  P.newline
  return (dir, count)

parse :: Parser Parsed
parse = P.many parseMotion

-- 5695
part1 :: Parsed -> Int
part1 parsed = S.size $ snd endRopeSet
  where endRopeSet = foldl (flip move) startRopeSet parsed

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
