module Day08
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Data.List (transpose)
import Data.Char (digitToInt)

import Helpers
import Parsers

type Height = Int
type Coord  = (Int, Int)
type Forest = [[Height]]
type Parsed = Forest

parseRow :: Parser [Height]
parseRow = do
  digits <- P.many P.digit
  return $ map digitToInt digits

parse :: Parser Parsed
parse = do
  rows <- P.sepBy parseRow P.newline
  return $ init rows

heightAt :: Forest -> Coord -> Height
heightAt f (x,y) = (f !! y) !! x

maxX f = length (head f) - 1
maxY f = length f - 1

isVisible :: Forest -> Coord -> Bool
isVisible f (x,y)
  | x == 0 || y == 0        = True
  | x == maxx || y == maxy  = True
  | otherwise               = all (< height) left || all (< height) right || all (< height) up || all (< height) down
  where maxx    = maxX f
        maxy    = maxY f
        height  = heightAt f (x,y)
        tf      = transpose f
        left    = take x (f !! y)
        right   = drop (x+1) (f !! y)
        up      = take y (tf !! x)
        down    = drop (y+1) (tf !! x)

-- | get number of trees in list that are visible from reference height
-- | can't see past a tree that's the same or higher than reference height
-- | trickier than a takeWhile, because we need to count the tree that stops our view
visibleTrees :: Height -> [Height] -> Int
visibleTrees _ [] = 0
visibleTrees h (t:ts)
  | t < h     = 1 + visibleTrees h ts
  | otherwise = 1

scenicScore :: Forest -> Coord -> Int
scenicScore f (x,y) = left * right * up * down
  where height  = heightAt f (x,y)
        maxx    = maxX f
        maxy    = maxY f
        left    = visibleTrees height $ reverse [ heightAt f (x',y) | x' <- [0..x-1] ]
        right   = visibleTrees height [ heightAt f (x',y) | x' <- [x+1..maxx] ]
        up      = visibleTrees height $ reverse [ heightAt f (x,y') | y' <- [0..y-1] ]
        down    = visibleTrees height [ heightAt f (x,y') | y' <- [y+1..maxy] ]

part1 :: Forest -> Int
part1 forest = length $ filter id [ isVisible forest (x,y) | x <- [0..maxX forest], y <- [0..maxY forest] ]

part2 :: Forest -> Int
part2 forest = maximum [ scenicScore forest (x,y) | x <- [0..maxX forest], y <- [0..maxY forest] ]

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  print $ part1 parsed
  print $ part2 parsed
