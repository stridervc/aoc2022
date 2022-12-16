module Day14
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import qualified Data.Map.Strict as M

import Control.Monad.State
import Control.Monad (when)

import Parsers
import Helpers

data CaveTile   = Air | Rock | Sand deriving (Eq, Show)
type Cave       = M.Map Coord CaveTile
data CaveStatus = AtRest | Falling | FallingForever | SpawnBlocked deriving (Eq, Show)
type RockPath   = [Coord]
type Parsed     = [RockPath]

parseRockPath :: Parser RockPath
parseRockPath = P.sepBy parseCoord (P.string " -> ")

parse :: Parser Parsed
parse = init <$> P.sepBy parseRockPath P.newline

setRock :: RockPath -> Cave -> Cave
setRock [] cave   = cave
setRock [_] cave  = cave
setRock (start:end:path) cave
  | sy == ey && sx > ex = continue $ setRock [end,start] cave
  | sx == ex && sy > ey = continue $ setRock [end,start] cave
  | sy == ey  = continue $ foldr (`M.insert` Rock) cave [ (x,sy) | x <- [sx..ex] ]
  | sx == ex  = continue $ foldr (`M.insert` Rock) cave [ (sx,y) | y <- [sy..ey] ]
  | otherwise = error "Shouldn't happen"
  where sx        = fst start
        sy        = snd start
        ex        = fst end
        ey        = snd end
        continue  = setRock (end:path)

-- | max y == lowest rock that's not the infinite floor
caveMaxY :: Cave -> Int
caveMaxY cave = maximum $ map snd $ M.keys $ M.filter (== Rock) cave

-- | default to Air
-- | if infinite == True, there is an infinite floor at y+2 below the lowest rock
lookupTile :: Bool -> Coord -> Cave -> CaveTile
lookupTile infinite (x,y) cave
  | not infinite  = M.findWithDefault Air (x,y) cave
  | y == maxy + 2 = Rock
  | otherwise     = lookupTile False (x,y) cave
  where maxy      = caveMaxY cave

-- | used for testing, print the cave
drawCave :: Bool -> Cave -> IO ()
drawCave infinite cave = mapM_ putLine [miny..maxy]
  where coords            = M.keys cave
        minx              = minimum $ map fst coords
        maxx              = maximum $ map fst coords
        miny              = minimum $ map snd coords
        maxy  | infinite  = caveMaxY cave + 2
              | otherwise = caveMaxY cave
        tileChar Rock     = pixel
        tileChar Air      = '.'
        tileChar Sand     = 'o'
        putLine y         = putStrLn [ tileChar (lookupTile infinite (x,y) cave) | x <- [minx..maxx] ]

spawnSand :: Cave -> Cave
spawnSand = M.insert (500,0) Sand

canFall :: Bool -> Coord -> Cave -> Bool
canFall infinite (x,y) cave = lookup' (x, y+1) == Air || lookup' (x-1, y+1) == Air || lookup' (x+1, y+1) == Air
  where lookup' (x,y) = lookupTile infinite (x,y) cave

-- | advance the cave one step
stepCave :: Bool -> Cave -> (Cave, CaveStatus)
stepCave infinite cave
  | blocked && atrest                 = (spawnSand cave, SpawnBlocked)
  | atrest                            = (spawnSand cave, Falling)
  | y >= maxy && not infinite         = (removed, FallingForever)
  | lookup' (x, y+1)   == Air         = (fall (x, y+1),   Falling)
  | lookup' (x-1, y+1) == Air         = (fall (x-1, y+1), Falling)
  | lookup' (x+1, y+1) == Air         = (fall (x+1, y+1), Falling)
  | otherwise                         = error "Didn't expect to get here"
  where sands     = M.keys $ M.filter (== Sand) cave
        unsettled = filter (\pos -> canFall infinite pos cave) sands
        (x, y)    = head unsettled
        fall e    = M.insert (x,y) Air $ M.insert e Sand cave
        maxy      = maximum $ map snd $ M.keys cave
        removed   = M.insert (x,y) Air cave
        lookup' (x,y) = lookupTile infinite (x,y) cave
        atrest        = null unsettled
        blocked       = not $ canFall False (500,0) cave

-- | advance the cave until it can't advance no more
runCave :: Bool -> Cave -> Cave
runCave infinite cave
  | status == AtRest          = runCave infinite $ spawnSand cave
  | status == FallingForever && not infinite = cave'
  | status == SpawnBlocked    = spawnSand cave
  | otherwise                 = runCave infinite cave'
  where (cave', status) = stepCave infinite cave

-- | part 2 approach, use breadth first search style queueing to fill the cave
-- | with sand from the spawn point downwards
data CaveState = CaveState
  { stateCave   :: Cave
  , stateQueue  :: [Coord]
  } deriving (Eq, Show)

newCaveState :: Cave -> CaveState
newCaveState cave = CaveState cave [ (500,0) ]

enqueue :: Coord -> State CaveState ()
enqueue coord = do
  state <- get
  queue <- gets stateQueue
  if coord `elem` queue then
    return ()
  else
    put $ state { stateQueue = queue <> [coord] }

pop :: State CaveState Coord
pop = do
  state <- get
  queue <- gets stateQueue
  put $ state { stateQueue = tail queue }
  return $ head queue

putSand :: Coord -> State CaveState ()
putSand coord = do
  state <- get
  cave  <- gets stateCave
  put $ state { stateCave = M.insert coord Sand cave }

fillSand :: State CaveState ()
fillSand = do
  cave  <- gets stateCave
  queue <- gets stateQueue

  if null queue then
    return ()
  else do
    (x,y) <- pop
    putSand (x,y)
    let maxy = caveMaxY cave + 2
    if y == maxy then
      return ()
    else do
      when (lookupTile True (x-1,y+1) cave == Air) (enqueue (x-1,y+1))
      when (lookupTile True (x,  y+1) cave == Air) (enqueue (x,  y+1))
      when (lookupTile True (x+1,y+1) cave == Air) (enqueue (x+1,y+1))
      fillSand

part1 :: Parsed -> IO ()
part1 paths = do
  -- drawCave False $ runCave False cave
  print $ M.size $ M.filter (== Sand) $ runCave False cave
  where cave  = foldr setRock mempty paths

part2 :: Parsed -> IO ()
part2 paths = do
  -- drawCave True cave'
  print $ M.size $ M.filter (== Sand) cave'
  where cave  = foldr setRock mempty paths
        cave' = stateCave $ execState fillSand (newCaveState cave)

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  part1 parsed
  part2 parsed
