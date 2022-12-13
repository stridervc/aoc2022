module Day12
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Char (ord)
import Data.List (sortOn, nub)
import Data.Maybe (fromJust)
import Control.Monad.State

import Parsers

type Coord      = (Int, Int)
type Start      = Coord
type End        = Coord
type HeightMap  = M.Map Coord Int -- ^ Trying a map instead of [[Int]]
type Visited    = S.Set Coord
type Path       = [Coord]

type Parsed = (Start, End, HeightMap)

addChar :: Coord -> Char -> Parsed -> Parsed
addChar coord 'S' (start, end, hmap) = addChar coord 'a' (coord, end, hmap)
addChar coord 'E' (start, end, hmap) = addChar coord 'z' (start, coord, hmap)
addChar coord c   (start, end, hmap) = (start, end, M.insert coord (ord c - ord 'a') hmap)

parse :: Parser Parsed
parse = do
  rows <- P.sepBy (P.many P.alphaNum) P.newline
  let maxx = length (head rows) - 1
  let maxy = length rows - 2
  return $ foldl (\p (coord, c) -> addChar coord c p) ((0,0),(0,0),mempty) [ ((x,y), (rows!!y)!!x) | x <- [0..maxx], y <- [0..maxy] ]

dfs :: Start -> End -> HeightMap -> Visited -> Path -> Path
dfs start@(sx, sy) end hmap visited path
  | start == end  = path
  | null paths    = path
  | otherwise     = path <> [start] <> head paths
  where Just height = M.lookup (sx, sy) hmap
        neighbours  = [ (sx+dx, sy+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 && dy /= 0 ]
        options     = [ coord | coord <- neighbours, coord `M.member` hmap, coord `S.notMember` visited, fromJust (M.lookup coord hmap) <= height+1 ]
        visited'    = S.insert start visited
        paths       = sortOn length $ filter (not . null) $ map (\s -> dfs s end hmap visited' path) options

bfs :: Start -> End -> HeightMap -> Visited -> [Coord] -> Path -> Path
bfs start@(sx,sy) end hmap visited queue path
  | null queue    = path
  | null options  = path
  | hq == end     = path <> [hq]
  | otherwise     = bfs ho end hmap visited'' queue' (path <> [ho])
  where hq          = head queue
        Just height = M.lookup start hmap
        visited'    = S.insert start visited
        neighbours  = [ (sx+dx, sy+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 && dy /= 0 ]
        options     = [ coord | coord <- neighbours, coord `M.member` hmap, coord `S.notMember` visited, fromJust (M.lookup coord hmap) <= height+1 ]
        ho          = head options
        visited''   = S.insert ho visited'
        queue'      = queue <> [ho]

data SearchState = SearchState
  { stateVisited  :: S.Set Coord  -- ^ Keep track of visited nodes
  , stateQueue    :: [Coord]      -- ^ FIFO queue of nodes to visit
  , statePath     :: Path         -- ^ Keep track of found path so far
  , stateHMap     :: HeightMap    -- ^ The heightmap we're searching through
  } deriving (Eq, Show)

defaultSearchState :: HeightMap -> SearchState
defaultSearchState = SearchState mempty mempty mempty

-- | add to queue if not already visited
enqueue :: Coord -> State SearchState ()
enqueue coord = do
  state   <- get
  visited <- gets stateVisited
  if coord `S.member` visited then
    return ()
  else
    put $ state { stateQueue = stateQueue state <> [coord] }

-- | mark as visited
visit :: Coord -> State SearchState ()
visit coord = do
  state   <- get
  visited <- gets stateVisited
  put $ state { stateVisited = S.insert coord visited }

pop :: State SearchState Coord
pop = do
  state <- get
  queue <- gets stateQueue
  put $ state { stateQueue = tail queue }
  return $ head queue

-- | get list of neightbours that are valid and haven't been visited
validNeighbours :: Coord -> State SearchState [Coord]
validNeighbours (x,y) = do
  visited <- gets stateVisited
  hmap    <- gets stateHMap
  return [ coord  | coord <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                  , coord `M.member` hmap
                  , coord `S.notMember` visited
                  ]

addToPath :: Coord -> State SearchState ()
addToPath coord = do
  state <- get
  path  <- gets statePath
  put $ state { statePath = path <> [coord] }

search :: Coord -> Coord -> State SearchState Bool
search start end = do
  visited <- gets stateVisited
  queue   <- gets stateQueue
  path    <- gets statePath

  enqueue start
  visit start
  next <- pop
  if next == end then do
    addToPath next
    return True
  else do
    neighbours <- validNeighbours start
    mapM_ enqueue neighbours
    search next end

--part1 :: Parsed -> Int
part1 (start, end, hmap) = do
  let state = execState (search start end) (defaultSearchState hmap)
  statePath state

part2 :: Parsed -> String
part2 parsed = "Not yet implemented"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile False day parse
  print $ part1 parsed
  print $ part2 parsed
