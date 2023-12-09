{-# LANGUAGE CPP #-}

module Attoparsec.Data.Implicit where

import qualified Attoparsec.Data.Explicit as A
import Attoparsec.Data.Prelude
import qualified Data.Attoparsec.Text as B

-- |
-- Provides the default lenient parser for a type.
--
-- By convention, the parser should not check for the end of input.
class LenientParser a where
  lenientParser :: B.Parser a

#define INSTANCE(TYPE, FUNCTION) instance LenientParser TYPE where {{-# INLINE lenientParser #-}; lenientParser = FUNCTION;}

-- | Consumes all the remaining input.
INSTANCE (Text, A.text)

-- | Consumes all the remaining input, encoding it using UTF8.
INSTANCE (ByteString, A.utf8Bytes)
INSTANCE (Char, A.char)
INSTANCE (Bool, A.bool)
INSTANCE (Integer, A.signedIntegral)
INSTANCE (Int, A.signedIntegral)
INSTANCE (Int8, A.signedIntegral)
INSTANCE (Int16, A.signedIntegral)
INSTANCE (Int32, A.signedIntegral)
INSTANCE (Int64, A.signedIntegral)
INSTANCE (Word, A.unsignedIntegral)
INSTANCE (Word8, A.unsignedIntegral)
INSTANCE (Word16, A.unsignedIntegral)
INSTANCE (Word32, A.unsignedIntegral)
INSTANCE (Word64, A.unsignedIntegral)
INSTANCE (Double, A.double)
INSTANCE (Scientific, A.scientific)
INSTANCE (TimeOfDay, A.timeOfDayInISO8601)
INSTANCE (Day, A.dayInISO8601)
INSTANCE (TimeZone, A.timeZoneInISO8601)
INSTANCE (UTCTime, A.utcTimeInISO8601)
INSTANCE (DiffTime, A.diffTime)
INSTANCE (NominalDiffTime, A.nominalDiffTime)
INSTANCE (String, A.string)
INSTANCE (UUID, A.uuid)

#undef INSTANCE
