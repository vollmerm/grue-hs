-- | The story dictionary and lexical analysis.
--
-- The dictionary lives in static memory and maps encoded words to
-- entry addresses; the game's parser attaches meaning to the data
-- bytes of each entry.  Lexical analysis splits player input into
-- words using the dictionary's separator characters, ready to be
-- looked up and written into a parse table by the @read@ and
-- @tokenise@ opcodes.
module Grue.Dictionary
  ( Dictionary (..)
  , readDictionary
  , entryAddr
  , lookupWord
  , allWords
  , tokenize
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Grue.Header
import Grue.Memory
import Grue.ZString

-- | The layout of a dictionary table, read from its short header.
data Dictionary = Dictionary
  { dictSeparators :: [Char]
  -- ^ Word-separator characters.  Each separator both divides words
  -- and counts as a word in its own right.
  , dictEntryLength :: Int
  -- ^ Length in bytes of one entry, encoded text plus data.
  , dictEntryCount :: Int
  -- ^ Number of entries.
  , dictEntriesAddr :: Int
  -- ^ Byte address of the first entry.
  }
  deriving (Eq, Show)

-- | Read the layout of the standard dictionary, whose address is given
-- in the story header.
readDictionary :: Memory -> Header -> Dictionary
readDictionary mem hdr =
  Dictionary
    { dictSeparators = seps
    , dictEntryLength = fromIntegral (peekByte mem (base + 1 + n))
    , dictEntryCount = fromIntegral (peekWord mem (base + 2 + n))
    , dictEntriesAddr = base + 4 + n
    }
  where
    base = dictionaryAddr hdr
    n = fromIntegral (peekByte mem base)
    codes = [peekByte mem (base + 1 + i) | i <- [0 .. n - 1]]
    seps = mapMaybe (zsciiToChar . fromIntegral) codes

-- | The byte address of a dictionary entry, by index.
entryAddr :: Dictionary -> Int -> Int
entryAddr dict i = dictEntriesAddr dict + i * dictEntryLength dict

-- | The encoded text of a dictionary entry: two words in versions 1 to
-- 3, three from version 4.
entryKey :: Memory -> Header -> Dictionary -> Int -> [Word16]
entryKey mem hdr dict i =
  [peekWord mem (entryAddr dict i + 2 * k) | k <- [0 .. keyWords - 1]]
  where
    keyWords = if zVersion hdr <= 3 then 2 else 3

-- | Look up a word, returning the byte address of its dictionary entry
-- if present.  Entries are stored in ascending order of their encoded
-- text, so the search is a binary chop.
lookupWord :: Memory -> Header -> Dictionary -> Text -> Maybe Int
lookupWord mem hdr dict word = go 0 (dictEntryCount dict - 1)
  where
    key = encodeWord hdr word
    go lo hi
      | lo > hi = Nothing
      | otherwise = case compare key (entryKey mem hdr dict mid) of
          EQ -> Just (entryAddr dict mid)
          LT -> go lo (mid - 1)
          GT -> go (mid + 1) hi
      where
        mid = (lo + hi) `div` 2

-- | Decode the text of every dictionary entry, in table order.  Useful
-- for inspecting a story's vocabulary.
allWords :: Memory -> Header -> Dictionary -> [Text]
allWords mem hdr dict =
  [ decodeStringAt mem hdr (entryAddr dict i)
  | i <- [0 .. dictEntryCount dict - 1]
  ]

-- | Split input text into words, returning each with its character
-- offset in the input.  Spaces divide words and are discarded;
-- separator characters divide words and are kept as one-character
-- words, so @\"fred,go  fishing\"@ becomes @fred@, @\",\"@, @go@,
-- @fishing@.
tokenize :: Dictionary -> Text -> [(Int, Text)]
tokenize dict input = go 0 (T.unpack input)
  where
    seps = dictSeparators dict
    isSep c = c `elem` seps
    go _ [] = []
    go i (c : cs)
      | c == ' ' = go (i + 1) cs
      | isSep c = (i, T.singleton c) : go (i + 1) cs
      | otherwise = (i, T.pack word) : go (i + length word) rest
      where
        (word, rest) = break (\x -> x == ' ' || isSep x) (c : cs)
