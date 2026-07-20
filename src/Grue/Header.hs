-- | The Z-machine story file header.
--
-- The header occupies the first 64 bytes of memory and describes where
-- the interpreter can find the dictionary, object table, global
-- variables and so on.  This module reads those fields into a 'Header'
-- snapshot at load time; all of the captured fields are static for the
-- lifetime of a story.  Dynamic header locations (such as the flags)
-- are read and written through 'Memory' directly.
module Grue.Header
  ( Header (..)
  , readHeader

    -- * Address translation
  , packedToByte

    -- * Integrity
  , computeChecksum
  , checksumValid
  ) where

import Data.ByteString qualified as BS
import Data.Word (Word16)
import Grue.Memory

-- | Static header fields, captured once when a story is loaded.
data Header = Header
  { zVersion :: !Int
  -- ^ Z-machine version number, 1 to 8.
  , highMemBase :: !Int
  -- ^ Byte address of the base of high memory.
  , initialPC :: !Int
  -- ^ Initial program counter (byte address, versions 1 to 5).
  , dictionaryAddr :: !Int
  -- ^ Byte address of the dictionary.
  , objectTableAddr :: !Int
  -- ^ Byte address of the object table.
  , globalsAddr :: !Int
  -- ^ Byte address of the global variables table.
  , staticBase :: !Int
  -- ^ Byte address of the base of static memory.  Addresses below
  -- this are dynamic memory, which the game may write to.
  , abbreviationsAddr :: !Int
  -- ^ Byte address of the abbreviations table.
  , fileLength :: !Int
  -- ^ Story file length in bytes, as declared in the header.
  , checksum :: !Word16
  -- ^ Checksum declared in the header.
  }
  deriving (Eq, Show)

-- | Read the header fields from story memory.
readHeader :: Memory -> Header
readHeader mem =
  Header
    { zVersion = version
    , highMemBase = word 0x04
    , initialPC = word 0x06
    , dictionaryAddr = word 0x08
    , objectTableAddr = word 0x0a
    , globalsAddr = word 0x0c
    , staticBase = word 0x0e
    , abbreviationsAddr = word 0x18
    , fileLength = word 0x1a * lengthScale version
    , checksum = peekWord mem 0x1c
    }
  where
    version = fromIntegral (peekByte mem 0x00)
    word = fromIntegral . peekWord mem

-- | The multiplier converting the header's stored file length to bytes.
lengthScale :: Int -> Int
lengthScale version
  | version <= 3 = 2
  | version <= 5 = 4
  | otherwise = 8

-- | Convert a packed address to a byte address.  Packed addresses are
-- how routine and string locations are stored in a single word; the
-- scale factor depends on the version.  Versions 6 and 7, which add
-- separate routine and string offsets, are not yet supported.
packedToByte :: Header -> Word16 -> Int
packedToByte hdr packed = scale * fromIntegral packed
  where
    scale = case zVersion hdr of
      v
        | v <= 3 -> 2
        | v <= 5 -> 4
        | v == 8 -> 8
      v -> error ("Grue.Header.packedToByte: unsupported version " ++ show v)

-- | Compute the story checksum: the sum, modulo 0x10000, of the
-- original story bytes from address 0x40 up to the declared file
-- length.  Writes made during play are ignored, as the standard
-- requires.
computeChecksum :: Memory -> Header -> Word16
computeChecksum mem hdr = BS.foldl' add 0 region
  where
    region = BS.take (fileLength hdr - 0x40) (BS.drop 0x40 (originalBytes mem))
    add acc b = acc + fromIntegral b

-- | Whether the computed checksum matches the one declared in the
-- header (the test made by the @verify@ opcode).
checksumValid :: Memory -> Header -> Bool
checksumValid mem hdr = computeChecksum mem hdr == checksum hdr
