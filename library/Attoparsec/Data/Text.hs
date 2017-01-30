{-|
Parsers of Text input.
-}
module Attoparsec.Data.Text where

import Attoparsec.Data.Prelude
import Data.Attoparsec.Text as A


integral :: Integral a => Parser a
integral =
  signed decimal

double :: Parser Double
double =
  A.double

scientific :: Parser Scientific
scientific =
  A.scientific

{-|
Accepts any string interpretable as a boolean:
"1" or "0", "true" or "false", "yes" or "no", "y" or "n", "t" or "f".
Case-insensitive.
-}
bool :: Parser Bool
bool =
  anyChar >>= \case
    '0' -> return False
    '1' -> return True
    'f' -> asciiCI "alse" $> False <|> pure False
    'F' -> asciiCI "alse" $> False <|> pure False
    't' -> asciiCI "rue" $> True <|> pure True
    'T' -> asciiCI "rue" $> True <|> pure True
    'n' -> satisfy (inClass "oO") $> False <|> pure False
    'N' -> satisfy (inClass "oO") $> False <|> pure False
    'y' -> asciiCI "es" $> True <|> pure True
    'Y' -> asciiCI "es" $> True <|> pure True
    _ -> empty

char :: Parser Char
char =
  A.anyChar
