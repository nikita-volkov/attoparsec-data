{-# LANGUAGE CPP #-}
module Attoparsec.Data.Implicit where

import Attoparsec.Data.Prelude
import qualified Attoparsec.Data.Explicit as A
import qualified Data.Attoparsec.Text as B


{-|
Provides the default lenient parser for a type.
-}
class LenientParser a where
  lenientParser :: B.Parser a

#define INSTANCE(TYPE, FUNCTION) instance LenientParser TYPE where {{-# INLINE lenientParser #-}; lenientParser = FUNCTION;}

INSTANCE(Bool, A.bool)
INSTANCE(Char, A.char)
INSTANCE(TimeOfDay, A.timeOfDayInISO8601)
INSTANCE(Day, A.dayInISO8601)
INSTANCE(TimeZone, A.timeZoneInISO8601)
INSTANCE(UTCTime, A.utcTimeInISO8601)
INSTANCE(Integer, A.signedIntegral)
INSTANCE(Int, A.signedIntegral)
INSTANCE(Int8, A.signedIntegral)
INSTANCE(Int16, A.signedIntegral)
INSTANCE(Int32, A.signedIntegral)
INSTANCE(Int64, A.signedIntegral)
INSTANCE(Word, A.unsignedIntegral)
INSTANCE(Word8, A.unsignedIntegral)
INSTANCE(Word16, A.unsignedIntegral)
INSTANCE(Word32, A.unsignedIntegral)
INSTANCE(Word64, A.unsignedIntegral)
INSTANCE(Double, A.double)
INSTANCE(Scientific, A.scientific)

#undef INSTANCE
