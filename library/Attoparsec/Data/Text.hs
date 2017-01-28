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

boolFromOneOrZero :: Parser Bool
boolFromOneOrZero =
  fromJust <$> satisfyWith asBoolMaybe isJust
  where
    asBoolMaybe =
      \case
        '0' -> Just False
        '1' -> Just True
        _ -> Nothing

{-|
Accepts any string interpretable as a boolean:
"1" or "0", "true" or "false", "yes" or "no", "y" or "n", "t" or "f".
Case-insensitive.
-}
bool :: Parser Bool
bool =
  one <|> zero <|> true <|> false <|> yes <|> no <|> y <|> n <|> t <|> f
  where
    one =
      A.char '1' $> True
    zero =
      A.char '0' $> False
    true =
      asciiCI "true" $> True
    false =
      asciiCI "false" $> False
    yes =
      asciiCI "yes" $> True
    no =
      asciiCI "no" $> False
    t =
      satisfy (inClass "tT") $> True
    f =
      satisfy (inClass "fF") $> False
    y =
      satisfy (inClass "yY") $> True
    n =
      satisfy (inClass "nN") $> False

char :: Parser Char
char =
  A.anyChar
