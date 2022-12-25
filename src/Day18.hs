module Day18
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Parsers
import Helpers

type Coord3D  = (Int,Int,Int)
type Parsed   = [Coord3D]

parseCoord3D :: Parser Coord3D
parseCoord3D = do
  x <- parseInt
  P.char ','
  y <- parseInt
  P.char ','
  z <- parseInt
  P.newline
  return (x,y,z)

parse :: Parser Parsed
parse = P.many parseCoord3D

numNeighbours :: Parsed -> Coord3D -> Int
numNeighbours input (x,y,z) = length $ filter id $ map (`elem` input) neighbours
  where deltas      = [ -1, 1 ]
        neighbours  = [ (x+d,y,z) | d <- deltas ] <> [ (x,y+d,z) | d <- deltas ] <> [ (x,y,z+d) | d <- deltas ]

coordX (x,_,_)  = x
coordY (_,y,_)  = y
coordZ (_,_,z)  = z

minX = minimum . map coordX
maxX = maximum . map coordX
minY = minimum . map coordY
maxY = maximum . map coordY
minZ = minimum . map coordZ
maxZ = maximum . map coordZ

-- airPockets is the wrong approach, since air pockets can be more than 1 cube big
-- have to find a way to count external surfaces only
airPockets :: Parsed -> [Coord3D]
airPockets input = [ (x,y,z) | x <- xs, y <- ys, z <- zs, (x,y,z) `notElem` input && numNeighbours input (x,y,z) == 6 ]
  where xs  = [ minX input .. maxX input ]
        ys  = [ minY input .. maxY input ]
        zs  = [ minZ input .. maxZ input ]

part1 :: Parsed -> IO ()
part1 parsed = print $ sum $ map ((6-) . numNeighbours parsed) parsed

-- 4058 too high
part2 :: Parsed -> IO ()
part2 parsed = print $ area - 6 * pockets
  where area    = sum $ map ((6-) . numNeighbours parsed) parsed
        pockets = length $ airPockets parsed

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  part1 parsed
  part2 parsed
