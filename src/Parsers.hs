module Parsers
  ( parseInt
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

parseNegInt :: Parser Int
parseNegInt = do
  P.char '-'
  num <- P.many1 P.digit
  return $ read num * (-1)

parseInt :: Parser Int
parseInt = P.choice
  [ parseNegInt
  , do
    num <- P.many1 P.digit
    return $ read num
  ]
