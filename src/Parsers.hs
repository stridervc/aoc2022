module Parsers
  ( inputfile
  , testfile
  , parseFile
  , parseInt
  ) where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as SP

import Text.Parsec.String (Parser)

inputfile :: String -> FilePath
inputfile day = concat [ "./inputs/input", day, ".txt" ]

testfile :: String -> FilePath
testfile day = concat [ "./inputs/test", day, ".txt" ]

parseFile :: Bool -> String -> Parser a -> IO a
parseFile real day parser = do
  parse <- SP.parseFromFile parser filename
  case parse of
    Left e        -> error $ show e
    Right parsed  -> return parsed
  where filename  | real      = inputfile day
                  | otherwise = testfile day

parseNegInt :: Integral a => Read a => Parser a
parseNegInt = do
  P.char '-'
  num <- P.many1 P.digit
  return $ read num * (-1)

parseInt :: Integral a => Read a => Parser a
parseInt = P.choice
  [ parseNegInt
  , do
    num <- P.many1 P.digit
    return $ read num
  ]
