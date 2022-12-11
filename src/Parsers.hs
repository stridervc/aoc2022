module Parsers
  ( parseInt
  ) where

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

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
