module Day15
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import qualified Data.Map.Strict as M

import Parsers
import Helpers

data Sensor = Sensor
  { sensorCoord   :: Coord
  , sensorBeacon  :: Coord
  } deriving (Eq, Show)

type Tunnels  = M.Map Coord Char
type Parsed   = [Sensor]

parseSensor :: Parser Sensor
parseSensor = do
  P.string "Sensor at x="
  sx <- parseInt
  P.string ", y="
  sy <- parseInt
  P.string ": closest beacon is at x="
  bx <- parseInt
  P.string ", y="
  by <- parseInt
  P.newline
  return $ Sensor
    { sensorCoord   = (sx, sy)
    , sensorBeacon  = (bx, by)
    }

parse :: Parser Parsed
parse = P.many parseSensor

addSensor :: Sensor -> Tunnels -> Tunnels
addSensor sensor tunnels = M.insert sk 'S' $ M.insert bk 'B' tunnels
  where sk  = sensorCoord sensor
        bk  = sensorBeacon sensor

tunnelsFromSensors :: Parsed -> Tunnels
tunnelsFromSensors = foldr addSensor mempty

-- | Default to emptiness
tunnelLookup :: Coord -> Tunnels -> Char
tunnelLookup = M.findWithDefault '.'

markCoverage :: Sensor -> Tunnels -> Tunnels
markCoverage sensor tunnels = foldr mark tunnels [ (x,y)  | x <- [sx-distance..sx+distance]
                                                          , y <- [sy-distance..sy+distance]
                                                          , delta x sx + delta y sy <= distance
                                                          ]
  where mark coord tunnels  | tunnelLookup coord tunnels == '.' = M.insert coord '#' tunnels
                            | otherwise                         = tunnels
        (sx, sy)            = sensorCoord sensor
        (bx, by)            = sensorBeacon sensor
        delta a b           | a < b     = b - a
                            | otherwise = a - b
        distance            = delta sx bx + delta sy by

markCoverages :: [Sensor] -> Tunnels -> Tunnels
markCoverages sensors tunnels = foldr markCoverage tunnels sensors

printTunnels :: Tunnels -> IO ()
printTunnels tunnels = mapM_ printLine [ miny..maxy ]
  where minx  = minimum $ map fst $ M.keys tunnels
        maxx  = maximum $ map fst $ M.keys tunnels
        miny  = minimum $ map snd $ M.keys tunnels
        maxy  = maximum $ map snd $ M.keys tunnels
        printLine y = putStrLn [ tunnelLookup (x,y) tunnels | x <- [minx..maxx] ]

part1 :: Parsed -> IO ()
part1 sensors = do
  -- print $ length $ M.keys $  M.filterWithKey (\k a -> snd k == 2000000 && a == '#') tunnels
  printTunnels tunnels
  where tunnels = markCoverages sensors $ tunnelsFromSensors sensors

part2 :: Parsed -> IO ()
part2 parsed = do
  putStrLn "Not yet implemented"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile False day parse
  part1 parsed
  part2 parsed
