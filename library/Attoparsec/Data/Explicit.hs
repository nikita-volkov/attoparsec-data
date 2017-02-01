module Attoparsec.Data.Explicit
(
  char,
  text,
  utf8Bytes,
  bool,
  signedIntegral,
  unsignedIntegral,
  A.double,
  A.scientific,
  -- * Time
  B.timeOfDayInISO8601,
  B.dayInISO8601,
  B.timeZoneInISO8601,
  B.utcTimeInISO8601,
)
where

import Attoparsec.Data.Prelude hiding (bool)
import qualified Data.Attoparsec.Text as A
import qualified Attoparsec.Time as B
import qualified Data.Text.Encoding as C


char :: A.Parser Char
char =
  A.anyChar

{-|
Consumes all the remaining input.
-}
text :: A.Parser Text
text =
  A.takeText

{-|
Consumes all the remaining input, encoding it using UTF8.
-}
utf8Bytes :: A.Parser ByteString
utf8Bytes =
  C.encodeUtf8 <$> A.takeText

{-|
Accepts any string interpretable as a boolean:
"1" or "0", "true" or "false", "yes" or "no", "y" or "n", "t" or "f".
Case-insensitive.
-}
bool :: A.Parser Bool
bool =
  A.anyChar >>= \case
    '0' -> return False
    '1' -> return True
    'f' -> A.asciiCI "alse" $> False <|> pure False
    'F' -> A.asciiCI "alse" $> False <|> pure False
    't' -> A.asciiCI "rue" $> True <|> pure True
    'T' -> A.asciiCI "rue" $> True <|> pure True
    'n' -> A.satisfy (A.inClass "oO") $> False <|> pure False
    'N' -> A.satisfy (A.inClass "oO") $> False <|> pure False
    'y' -> A.asciiCI "es" $> True <|> pure True
    'Y' -> A.asciiCI "es" $> True <|> pure True
    _ -> empty

signedIntegral :: Integral a => A.Parser a
signedIntegral =
  A.signed A.decimal

unsignedIntegral :: Integral a => A.Parser a
unsignedIntegral =
  A.decimal
