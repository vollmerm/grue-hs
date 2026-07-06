-- | Encoding and decoding of Z-machine text.
--
-- Story text is stored as a sequence of 16-bit words, each packing
-- three 5-bit Z-characters; the top bit of the final word marks the end
-- of the string.  Z-characters select letters from one of three
-- alphabets, with shift characters, abbreviation references and a
-- two-character escape for arbitrary ZSCII codes.
--
-- This module implements the version 3 and later rules (shifts apply
-- to a single character; Z-characters 1 to 3 introduce abbreviations).
module Grue.ZString
  ( -- * Decoding
    decodeString
  , decodeStringAt

    -- * Encoding
  , encodeWord

    -- * ZSCII
  , zsciiToChar
  , charToZscii
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char (chr, ord, toLower)
import Data.List (elemIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Grue.Header
import Grue.Memory

-- | The three Z-machine alphabets.
data Alphabet = A0 | A1 | A2
  deriving (Eq, Show)

-- | The alphabet rows for versions 2 and later, translating
-- Z-characters 6 to 31.  Positions 0 and 1 of 'alpha2' are placeholders:
-- Z-character 6 in A2 starts a ZSCII escape and 7 is a new-line, so
-- neither is looked up here.
alpha0, alpha1, alpha2 :: String
alpha0 = "abcdefghijklmnopqrstuvwxyz"
alpha1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
alpha2 = " \n0123456789.,!?_#'\"/\\-:()"

alphabetRow :: Alphabet -> String
alphabetRow A0 = alpha0
alphabetRow A1 = alpha1
alphabetRow A2 = alpha2

-- | Decode the Z-string starting at a byte address.  Returns the text
-- and the address just past the string's final word, which is where
-- execution resumes after an inline @print@.
decodeString :: Memory -> Header -> Int -> (Text, Int)
decodeString mem hdr addr = (translate mem hdr True zchars, end)
  where
    (zchars, end) = zcharsFrom mem addr

-- | Decode the Z-string at a byte address, discarding the end address.
decodeStringAt :: Memory -> Header -> Int -> Text
decodeStringAt mem hdr = fst . decodeString mem hdr

-- | Split the words of a Z-string into 5-bit Z-characters, also
-- returning the address just past the final word (the one with the top
-- bit set).
zcharsFrom :: Memory -> Int -> ([Int], Int)
zcharsFrom mem = go
  where
    go addr
      | testBit w 15 = (cs, addr + 2)
      | otherwise = let (rest, end) = go (addr + 2) in (cs ++ rest, end)
      where
        w = peekWord mem addr
        cs =
          [ fromIntegral (w `shiftR` 10) .&. 31
          , fromIntegral (w `shiftR` 5) .&. 31
          , fromIntegral w .&. 31
          ]

-- | Translate a stream of Z-characters to text.  The flag controls
-- whether abbreviations may be expanded; it is switched off while
-- expanding an abbreviation, which the standard forbids from nesting.
translate :: Memory -> Header -> Bool -> [Int] -> Text
translate mem hdr allowAbbrev = T.concat . go A0
  where
    go :: Alphabet -> [Int] -> [Text]
    go _ [] = []
    go alpha (c : cs) = case c of
      0 -> T.singleton ' ' : go A0 cs
      _ | c <= 3 -> abbreviation c cs
      4 -> go A1 cs
      5 -> go A2 cs
      6 | alpha == A2 -> zsciiEscape cs
      7 | alpha == A2 -> T.singleton '\n' : go A0 cs
      _ -> letter alpha c : go A0 cs

    -- An incomplete construction at the end of a string is ignored
    -- (it can arise from dictionary truncation).
    abbreviation _ [] = []
    abbreviation z (x : cs)
      | allowAbbrev = expandAbbrev mem hdr (32 * (z - 1) + x) : go A0 cs
      | otherwise = go A0 cs

    zsciiEscape (hi : lo : cs) = zsciiText (hi `shiftL` 5 .|. lo) : go A0 cs
    zsciiEscape _ = []

    letter alpha c = T.singleton (alphabetRow alpha !! (c - 6))

-- | Look up and decode entry @n@ of the abbreviations table.  Entries
-- hold word addresses (byte address divided by two).
expandAbbrev :: Memory -> Header -> Int -> Text
expandAbbrev mem hdr n = translate mem hdr False zchars
  where
    entry = abbreviationsAddr hdr + 2 * n
    addr = 2 * fromIntegral (peekWord mem entry)
    (zchars, _) = zcharsFrom mem addr

-- | Render a ZSCII output code as text.  Undefined output codes render
-- as nothing, per the standard's rule for the null character.
zsciiText :: Int -> Text
zsciiText code = maybe T.empty T.singleton (zsciiToChar (fromIntegral code))

-- | Convert a ZSCII output code to a character, if it has a printable
-- translation.  Codes 32 to 126 agree with ASCII; 155 to 223 use the
-- standard's default Unicode translation table.
zsciiToChar :: Word16 -> Maybe Char
zsciiToChar code = case fromIntegral code :: Int of
  13 -> Just '\n'
  c
    | c >= 32 && c <= 126 -> Just (chr c)
    | c >= 155 && c <= 154 + length defaultUnicode ->
        Just (defaultUnicode !! (c - 155))
  _ -> Nothing

-- | Convert a character to its ZSCII code, if it has one.  This is the
-- inverse of 'zsciiToChar' and is used when encoding player input.
charToZscii :: Char -> Maybe Word16
charToZscii ch = case ch of
  '\n' -> Just 13
  c
    | c >= ' ' && c <= '~' -> Just (fromIntegral (ord c))
    | otherwise ->
        (\i -> fromIntegral (155 + i)) <$> elemIndex c defaultUnicode

-- | The default translation of the ZSCII "extra characters" 155 to 223
-- into Unicode, from Table 1 of the standard.
defaultUnicode :: [Char]
defaultUnicode =
  "äöüÄÖÜß»«ëïÿËÏáéíóúýÁÉÍÓÚÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛåÅøØãñõÃÑÕæÆçÇþðÞÐ£œŒ¡¿"

-- | Encode text as dictionary-lookup Z-characters: 6 Z-characters in
-- two words for versions 1 to 3, or 9 in three words for version 4 and
-- later.  Text is lower-cased, over-long text is truncated, and short
-- text is padded with Z-character 5, as the standard requires.
encodeWord :: Header -> Text -> [Word16]
encodeWord hdr word = packWords (take count (zchars ++ repeat 5))
  where
    count = if zVersion hdr <= 3 then 6 else 9
    zchars = concatMap encodeChar (T.unpack (T.toLower word))

-- | The Z-characters for a single input character: a plain A0 letter,
-- a shift into A2, or a full ZSCII escape.
encodeChar :: Char -> [Int]
encodeChar c
  | c == ' ' = [0]
  | Just i <- elemIndex c alpha0 = [6 + i]
  | Just i <- elemIndex c (drop 2 alpha2) = [5, 8 + i]
  | otherwise = case charToZscii (toLower c) of
      Just code ->
        let z = fromIntegral code
         in [5, 6, (z `shiftR` 5) .&. 31, z .&. 31]
      Nothing -> []

-- | Pack Z-characters three to a word, setting the end bit on the last.
packWords :: [Int] -> [Word16]
packWords (a : b : c : rest) = word : packWords rest
  where
    packed = a `shiftL` 10 .|. b `shiftL` 5 .|. c
    word
      | null rest = fromIntegral packed .|. 0x8000
      | otherwise = fromIntegral packed
packWords _ = []
