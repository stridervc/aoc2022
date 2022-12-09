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
type Rope   = [Coord]       -- ^ Leading knot is at head of rope
type Motion = (Char, Int)
type Parsed = [Motion]

type TailCoords = S.Set Coord
type RopeSet    = (Rope, TailCoords)

ropeSetOfLength :: Int -> RopeSet
ropeSetOfLength i = (replicate i (0,0), S.singleton (0, 0))

-- | follower catches up to leader (if necessary)
-- | do the same for the rest of the knots
-- | if follower is the last knot, add coords to set
catchup :: RopeSet -> RopeSet
catchup (l:f:ks, set)
  | abs dx <= 1 && abs dy <= 1  = let (ks', set') = catchup (f:ks, set) in (l:f:tail ks', set')
  | otherwise = (l:f':tail ks', set'')
  where lx          = fst l
        ly          = snd l
        fx          = fst f
        fy          = snd f
        dx          = lx - fx
        dy          = ly - fy
        cy          | dy > 0    = 1
                    | dy < 0    = -1
                    | otherwise = 0
        cx          | dx > 0    = 1
                    | dx < 0    = -1
                    | otherwise = 0
        f'          = (fx+cx, fy+cy)
        (ks', set') = catchup (f':ks, set)
        set''       | null ks   = S.insert f' set'
                    | otherwise = set'
catchup rs  = rs

-- | move leader, have followers catch up
move :: Motion -> RopeSet -> RopeSet
move (_, 0) rs                    = rs
move ('U', c) ((lx, ly):ks, set)  = move ('U', c-1) $ catchup ((lx, ly-1):ks, set)
move ('D', c) ((lx, ly):ks, set)  = move ('D', c-1) $ catchup ((lx, ly+1):ks, set)
move ('L', c) ((lx, ly):ks, set)  = move ('L', c-1) $ catchup ((lx-1, ly):ks, set)
move ('R', c) ((lx, ly):ks, set)  = move ('R', c-1) $ catchup ((lx+1, ly):ks, set)
move _ _                          = error "Unexpected motion in move"

parseMotion :: Parser Motion
parseMotion = do
  dir <- P.anyChar
  P.space
  count <- parseInt
  P.newline
  return (dir, count)

parse :: Parser Parsed
parse = P.many parseMotion

part1 :: Parsed -> Int
part1 parsed = S.size $ snd endRopeSet
  where endRopeSet = foldl (flip move) (ropeSetOfLength 2) parsed

part2 :: Parsed -> Int
part2 parsed = S.size $ snd endRopeSet
  where endRopeSet = foldl (flip move) (ropeSetOfLength 10) parsed

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  print $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
