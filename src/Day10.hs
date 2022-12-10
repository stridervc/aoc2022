module Day10
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import Helpers
import Parsers

data Instr    = Noop | AddX Int deriving (Eq, Show)
type Cycle    = Int
type Register = Int
type CPU      = (Cycle, Register)
type Parsed   = [Instr]

parseInstr :: Parser Instr
parseInstr = P.choice
  [ do
    P.string "noop"
    P.newline
    return Noop
  , do
    P.string "addx "
    i <- parseInt
    P.newline
    return $ AddX i
  ]

parse :: Parser Parsed
parse = P.many parseInstr

applyInstr :: Instr -> CPU -> CPU
applyInstr Noop     (cycle, reg)  = (cycle+1, reg)
applyInstr (AddX i) (cycle, reg)  = (cycle+2, reg+i)

regAtCycle :: [Instr] -> CPU -> Cycle -> Register
regAtCycle [] (_, reg) _  = reg
regAtCycle (instr:instrs) (cc, cr) cycle
  | cc' > cycle = cr
  | otherwise   = regAtCycle instrs (cc', cr') cycle
  where (cc', cr')  = applyInstr instr (cc, cr)

pixelAtCycle :: [Instr] -> CPU -> Cycle -> Char
pixelAtCycle instrs cpu cycle
  | scanpos >= reg-1 && scanpos <= reg+1  = pixel
  | otherwise                             = ' '
  where scanpos = (cycle-1) `mod` 40
        reg     = regAtCycle instrs cpu cycle

part1 :: Parsed -> Int
part1 instrs = sum [ c * regAtCycle instrs (1,1) c | c <- [20,60..220] ]

part2 :: Parsed -> [String]
part2 instrs  = [ take 40 (drop x pixels) | x <- [0,40..200] ]
  where pixels = [ pixelAtCycle instrs (1,1) c | c <- [1..] ]

solve :: String -> IO ()
solve input = do
  print $ part1 parsed
  mapM_ putStrLn $ part2 parsed
  where Right parsed  = P.parse parse "(input)" input
