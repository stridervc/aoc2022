module Day17
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

data Block      = FallingRock | Rock | Air deriving (Eq, Show)
data RockShape  = RockMinus | RockPlus | RockRL | RockTall | RockSquare deriving (Eq, Show, Enum)
type Chamber    = M.Map Coord Block
type Parsed     = String

data ChamberState = ChamberState
  { stateChamber  :: Chamber
  , stateRock     :: RockShape
  , stateRockPos  :: Coord
  , stateCount    :: Int
  , stateHighest  :: Int
  } deriving (Eq, Show)

defaultChamberState = ChamberState
  { stateChamber  = mempty
  , stateRock     = RockMinus
  , stateRockPos  = (2, -4)
  , stateCount    = 0
  , stateHighest  = 0
  }

parse :: Parser Parsed
parse = P.manyTill P.anyChar P.newline

nextShape :: RockShape -> RockShape
nextShape RockSquare  = RockMinus
nextShape shape       = succ shape

chamberLookup :: Coord -> Chamber -> Block
chamberLookup = M.findWithDefault Air

shapeStrings :: RockShape -> [String]
shapeStrings RockMinus  = [ "####" ]
shapeStrings RockPlus   = [ ".#."
                          , "###"
                          , ".#." ]
shapeStrings RockRL     = [ "..#"
                          , "..#"
                          , "###" ]
shapeStrings RockTall   = [ "#", "#", "#", "#" ]
shapeStrings RockSquare = [ "##"
                          , "##" ]

shapeWidth :: RockShape -> Int
shapeWidth = length . head . shapeStrings

shapeHeight :: RockShape -> Int
shapeHeight = length . shapeStrings

removeShape :: Coord -> RockShape -> State ChamberState ()
removeShape (x,y) shape = do
  state   <- get
  chamber <- gets stateChamber
  put $ state { stateChamber = removed chamber }
  where maxx            = shapeWidth shape - 1
        maxy            = shapeHeight shape - 1
        shapestr        = shapeStrings shape
        removed chamber = foldr (`M.insert` Air) chamber  [ (x+dx, y+dy) | dx <- [0..maxx], dy <- [0..maxy]
                                                          , (shapestr!!dy)!!dx == '#' ]

placeShape :: Coord -> RockShape -> State ChamberState ()
placeShape (x, y) shape = do
  state   <- get
  chamber <- gets stateChamber
  put $ state { stateChamber = placed chamber }
  where maxx            = shapeWidth shape - 1
        maxy            = shapeHeight shape - 1
        shapestr        = shapeStrings shape
        placed chamber  = foldr (`M.insert` Rock) chamber [ (x+dx, y+dy) | dx <- [0..maxx], dy <- [0..maxy]
                                                          , (shapestr!!dy)!!dx == '#' ]

setCoord :: Coord -> State ChamberState ()
setCoord coord = modify' (\s -> s { stateRockPos = coord })

canPlaceShape :: Coord -> RockShape -> State ChamberState Bool
canPlaceShape (x,y) shape = do
  chamber <- gets stateChamber

  let shapestr  = shapeStrings shape
  let maxx      = shapeWidth shape - 1
  let maxy      = shapeHeight shape - 1

  if (x < 0) || (x + maxx + 1 > 7) || (y + maxy + 1 > 0) then return False else
    return $ notElem Rock [ chamberLookup (x+dx, y+dy) chamber | dx <- [0..maxx], dy <- [0..maxy], (shapestr!!dy)!!dx == '#' ]

applyJet :: Char -> State ChamberState ()
applyJet jet = do
  state   <- get
  chamber <- gets stateChamber
  shape   <- gets stateRock
  (x,y)   <- gets stateRockPos

  let nx = if jet == '>' then x+1 else x-1
  removeShape (x,y) shape
  cando <- canPlaceShape (nx,y) shape
  if cando then do
    placeShape (nx,y) shape
    setCoord (nx,y)
  else put state

applyFall :: State ChamberState ()
applyFall = do
  shape   <- gets stateRock
  (x,y)   <- gets stateRockPos

  removeShape (x,y) shape
  placeShape (x,y+1) shape
  setCoord (x,y+1)

spawnRock :: State ChamberState ()
spawnRock = do
  chamber <- gets stateChamber
  shape   <- gets stateRock
  highest <- gets stateHighest

  setCoord (2, highest - shapeHeight shape - 3)
  placeShape (2, highest - shapeHeight shape - 3) shape

incCount :: State ChamberState ()
incCount = do
  count <- gets stateCount
  modify (\s -> s { stateCount = count+1 } )

nextRock :: State ChamberState ()
nextRock = do
  shape <- gets stateRock
  modify (\s -> s { stateRock = nextShape shape } )

canFall :: State ChamberState Bool
canFall = do
  state <- get
  shape <- gets stateRock
  (x,y) <- gets stateRockPos

  removeShape (x,y) shape
  cando <- canPlaceShape (x,y+1) shape
  put state
  return cando

setHighest :: State ChamberState ()
setHighest = do
  (_,y)   <- gets stateRockPos
  highest <- gets stateHighest

  when (y < highest) $ modify (\s -> s { stateHighest = y } )

runChamber :: Parsed -> State ChamberState ()
runChamber []      = return ()
runChamber (j:js)  = do
  count <- gets stateCount
  if count >= 2022 then return () else do
    applyJet j
    cando <- canFall
    if cando then do
     applyFall
     runChamber js
    else do
      incCount
      setHighest
      nextRock
      spawnRock
      runChamber js

printChamber :: Chamber -> IO ()
printChamber chamber = mapM_ putLine [miny..maxy]
  where coords        = M.keys chamber
        miny          = minimum $ map snd coords
        maxy          = -1
        putLine y     = putStrLn [ tileChar (chamberLookup (x,y) chamber) | x <- [0..6] ]
        tileChar Air  = '.'
        tileChar _    = '#'

part1 :: Parsed -> IO ()
part1 parsed = do
  printChamber $ stateChamber state
  print $ stateRockPos state
  print $ stateHighest state
  where input = cycle parsed
        state = execState (runChamber input) defaultChamberState

part2 :: Parsed -> IO ()
part2 parsed = do
  putStrLn "Not yet implemented"

solve :: String -> IO ()
solve day = do
  parsed <- parseFile True day parse
  part1 parsed
  part2 parsed
