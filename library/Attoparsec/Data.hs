module Attoparsec.Data where

import Attoparsec.Data.Prelude
import Data.Attoparsec.Text as A
import qualified Attoparsec.Time as B


{-|
Provides the default lenient parser for a type.
-}
class LenientParser a where
  lenientParser :: Parser a

{-|
Supports any string interpretable as a boolean:
"1" or "0", "true" or "false", "yes" or "no", "y" or "n", "t" or "f".
Case-insensitive.
-}
instance LenientParser Bool where
  {-# INLINABLE lenientParser #-}
  lenientParser =
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

instance LenientParser Char where
  {-# INLINE lenientParser #-}
  lenientParser =
    anyChar

{-|
Supports the ISO8601 format.
-}
instance LenientParser TimeOfDay where
  {-# INLINE lenientParser #-}
  lenientParser =
    B.timeOfDayInISO8601

{-|
Supports the ISO8601 format.
-}
instance LenientParser Day where
  {-# INLINE lenientParser #-}
  lenientParser =
    B.dayInISO8601

{-|
Supports the ISO8601 format.
-}
instance LenientParser TimeZone where
  {-# INLINE lenientParser #-}
  lenientParser =
    B.timeZoneInISO8601

{-|
Supports the ISO8601 format.
-}
instance LenientParser UTCTime where
  {-# INLINE lenientParser #-}
  lenientParser =
    B.utcTimeInISO8601

{-|
Supports any possibly signed decimal number.
-}
instance LenientParser Integer where
  {-# INLINE lenientParser #-}
  lenientParser =
    signed decimal

{-|
Supports any possibly signed decimal number.
-}
instance LenientParser Int where
  {-# INLINE lenientParser #-}
  lenientParser =
    signed decimal

{-|
Supports any possibly signed decimal number.
-}
instance LenientParser Word where
  {-# INLINE lenientParser #-}
  lenientParser =
    signed decimal

instance LenientParser Double where
  {-# INLINE lenientParser #-}
  lenientParser =
    double

instance LenientParser Scientific where
  {-# INLINE lenientParser #-}
  lenientParser =
    scientific
