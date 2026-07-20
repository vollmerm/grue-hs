-- | Z-machine story memory.
--
-- Memory is represented as the immutable bytes of the story file plus an
-- overlay of the writes made to dynamic memory.  Updates never mutate the
-- original story bytes, so snapshots of machine state are cheap to keep,
-- which makes features like undo and save straightforward.
--
-- Addresses are plain 'Int' byte offsets from the start of the story.
-- The Z-machine's packed and word addresses are resolved to byte
-- addresses by callers before touching memory.  Words are stored
-- big-endian, as required by the Z-machine standard.
module Grue.Memory
  ( Memory
  , fromStory
  , memorySize

    -- * Reading
  , peekByte
  , peekWord

    -- * Writing
  , pokeByte
  , pokeWord

    -- * Whole-story access
  , originalBytes
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Word (Word16, Word8)

-- | The addressable memory of a running story.
data Memory = Memory
  { memStory :: !ByteString
  -- ^ The story file as loaded, never modified.
  , memWrites :: !(IntMap Word8)
  -- ^ Bytes written since load, keyed by address.  A read consults
  -- this overlay first and falls back to the story bytes.
  }
  deriving (Eq, Show)

-- | Create memory from the raw bytes of a story file.
fromStory :: ByteString -> Memory
fromStory story = Memory {memStory = story, memWrites = IntMap.empty}

-- | Total number of addressable bytes (the story file length).
memorySize :: Memory -> Int
memorySize = BS.length . memStory

-- | The unmodified bytes of the story file, e.g. for computing the
-- header checksum.
originalBytes :: Memory -> ByteString
originalBytes = memStory

-- | Read the byte at an address.  Fails with an informative error on an
-- out-of-range address, which indicates either a corrupt story file or
-- an interpreter bug.
peekByte :: Memory -> Int -> Word8
peekByte mem addr
  | addr < 0 || addr >= memorySize mem =
      error ("Grue.Memory.peekByte: address out of range: " ++ show addr)
  | otherwise =
      IntMap.findWithDefault (BS.index (memStory mem) addr) addr (memWrites mem)

-- | Read the big-endian 16-bit word starting at an address.
peekWord :: Memory -> Int -> Word16
peekWord mem addr = hi `shiftL` 8 .|. lo
  where
    hi = fromIntegral (peekByte mem addr)
    lo = fromIntegral (peekByte mem (addr + 1))

-- | Write a byte at an address.  The argument order suits pipelines and
-- folds: @'pokeByte' addr value :: Memory -> Memory@.
pokeByte :: Int -> Word8 -> Memory -> Memory
pokeByte addr value mem
  | addr < 0 || addr >= memorySize mem =
      error ("Grue.Memory.pokeByte: address out of range: " ++ show addr)
  | otherwise = mem {memWrites = IntMap.insert addr value (memWrites mem)}

-- | Write a big-endian 16-bit word starting at an address.
pokeWord :: Int -> Word16 -> Memory -> Memory
pokeWord addr value =
  pokeByte addr hi . pokeByte (addr + 1) lo
  where
    hi = fromIntegral (value `shiftR` 8)
    lo = fromIntegral (value .&. 0xff)
