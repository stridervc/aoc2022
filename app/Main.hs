module Main where

import qualified Data.Map as M
import System.Environment (getArgs)
import System.Posix.Files (fileExist)

import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)

solutions = M.fromList
  [ ("01", Day01.solve)
  , ("02", Day02.solve)
  , ("03", Day03.solve)
  , ("04", Day04.solve)
  , ("05", Day05.solve)
  , ("06", Day06.solve)
  , ("07", Day07.solve)
  ]

solveSingle :: String -> IO ()
solveSingle day = do
  putStrLn $ "--- Day " ++ day ++ " ---"
  exist <- fileExist inputfile
  if exist then case M.lookup day solutions of
      Just solver -> readFile inputfile >>= solver >> putStrLn ""
      Nothing     -> putStrLn "Not yet implemented"
  else putStrLn "No input file"
  where inputfile = concat ["./inputs/input", day, ".txt"]

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ solveSingle $ M.keys solutions
    else mapM_ solveSingle args
