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
import Data.Maybe (fromJust, isNothing)
import Control.Monad.State

import Parsers

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

type Parents  = M.Map Coord Coord

data SearchState = SearchState
  { stateExplored :: S.Set Coord  -- ^ Keep track of explored nodes
  , stateQueue    :: [Coord]      -- ^ FIFO queue of nodes to visit
  , stateParents  :: Parents      -- ^ Parent nodes of nodes, used to find path
  , stateHMap     :: HeightMap    -- ^ The heightmap we're searching through
  } deriving (Eq, Show)

defaultSearchState :: Start -> HeightMap -> SearchState
defaultSearchState start = SearchState (S.singleton start) [start] mempty

-- | add to queue
enqueue :: Coord -> State SearchState ()
enqueue coord = do
  state   <- get
  put $ state { stateQueue = stateQueue state <> [coord] }

-- | mark as explored
explore :: Coord -> State SearchState ()
explore coord = do
  state     <- get
  explored  <- gets stateExplored
  put $ state { stateExplored = S.insert coord explored }

pop :: State SearchState Coord
pop = do
  state <- get
  queue <- gets stateQueue
  put $ state { stateQueue = tail queue }
  return $ head queue

setParent :: Coord -> Coord -> State SearchState ()
setParent node parent = do
  state   <- get
  parents <- gets stateParents
  put $ state { stateParents = M.insert node parent parents }

-- | get list of neighbours that are valid and haven't been explored
validNeighbours :: Coord -> State SearchState [Coord]
validNeighbours (x,y) = do
  explored  <- gets stateExplored
  hmap      <- gets stateHMap
  let maxh  = fromJust (M.lookup (x,y) hmap) + 1

  return [ coord  | coord <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                  , coord `M.member` hmap
                  , coord `S.notMember` explored
                  , fromJust (M.lookup coord hmap) <= maxh
                  ]

-- | bfs : breadth first search, keeping track of parents
search :: End -> State SearchState Bool
search end = do
  explored  <- gets stateExplored
  queue     <- gets stateQueue

  if null queue then
    return False
  else do
    node <- pop
    if node == end then do
      return True
    else do
      neighbours <- validNeighbours node
      mapM_ enqueue neighbours
      mapM_ explore neighbours
      mapM_ (`setParent` node) neighbours
      search end

getPath :: Coord -> SearchState -> [Coord] -> [Coord]
getPath node state path
  | isNothing parent  = []
  | otherwise         = node : getPath (fromJust parent) state path
  where parent  = M.lookup node $ stateParents state

part1 :: Parsed -> Int
part1 (start, end, hmap) = length $ getPath end state []
  where state = execState (search end) (defaultSearchState start hmap)

-- | Some starting points can never reach the end, their trips are length 0
part2 :: Parsed -> Int
part2 (_, end, hmap) = minimum $ filter (/=0) trips
  where starts  = M.keys $ M.filter (==0) hmap
        states  = map (\start -> execState (search end) (defaultSearchState start hmap)) starts
        trips   = map (\state -> length $ getPath end state []) states

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  print $ part1 parsed
  print $ part2 parsed
