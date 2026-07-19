{-# LANGUAGE OverloadedStrings #-}

-- | Reading and writing saved games in the Quetzal format.
--
-- Quetzal is the standard interchange format for Z-machine saves: an
-- IFF @FORM@ of type @IFZS@ holding the story identification
-- (@IFhd@), dynamic memory as an exclusive-or diff against the
-- original story with run-length compression (@CMem@), and the call
-- stack (@Stks@).  Files written here restore in other conforming
-- interpreters and vice versa.
module Grue.Quetzal
  ( saveState
  , restoreState
  ) where

import Data.Bits (popCount, shiftL, shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import Grue.Header
import Grue.Memory
import Grue.VM

-- | Serialize the machine into a Quetzal save.  The given address is
-- where execution resumes on restore: for version 3, the branch data
-- of the @save@ instruction being executed.
saveState :: VM -> Int -> ByteString
saveState vm resumePC =
  form
    [ chunk "IFhd" (ifhd vm resumePC)
    , chunk "CMem" (cmem vm)
    , chunk "Stks" (stks vm)
    ]

-- | Rebuild a machine from story file bytes and a Quetzal save.
-- Bookkeeping fields (output buffer, random generator) come out fresh;
-- the caller decides what survives from the machine being replaced.
restoreState :: ByteString -> ByteString -> Either String VM
restoreState story save = do
  body <- iffBody save
  let sections = chunksOf body
      base = boot story
      mem0 = vmMemory base
  ifhdBytes <- require "IFhd" sections
  checkStory story ifhdBytes
  mem <- case lookup "CMem" sections of
    Just bytes -> applyCMem mem0 (vmHeader base) bytes
    Nothing -> applyUMem mem0 (vmHeader base) =<< require "UMem" sections
  frames <- parseFrames =<< require "Stks" sections
  ordered <-
    maybe (Left "Quetzal: no stack frames") Right (nonEmpty (reverse frames))
  pure
    base
      { vmMemory = mem
      , vmFrames = ordered
      , vmPC = readW24 ifhdBytes 10
      }
  where
    require :: ByteString -> [(ByteString, ByteString)] -> Either String ByteString
    require name sections =
      maybe (Left ("Quetzal: missing " ++ show name ++ " chunk")) Right $
        lookup name sections

-- | The story identification chunk: release, serial, checksum, and
-- the resume address in three bytes.
ifhd :: VM -> Int -> ByteString
ifhd vm resumePC =
  BS.concat
    [ slice 0x02 2
    , slice 0x12 6
    , slice 0x1c 2
    , BS.pack (w24 resumePC)
    ]
  where
    slice from len = BS.take len (BS.drop from (originalBytes (vmMemory vm)))

-- | Check a save's identification bytes against the loaded story.
checkStory :: ByteString -> ByteString -> Either String ()
checkStory story ifhdBytes
  | BS.length ifhdBytes < 13 = Left "Quetzal: short IFhd chunk"
  | actual == expected = Right ()
  | otherwise = Left "Quetzal: save belongs to a different story"
  where
    expected = BS.take 10 ifhdBytes
    actual =
      BS.concat
        [ BS.take 2 (BS.drop 0x02 story)
        , BS.take 6 (BS.drop 0x12 story)
        , BS.take 2 (BS.drop 0x1c story)
        ]

-- | Dynamic memory, exclusive-ored with the original story and
-- run-length compressed: a zero byte is followed by a count of extra
-- zeroes.  Trailing zero runs are omitted.
cmem :: VM -> ByteString
cmem vm = BS.pack (encode (dropTrailingZeros diffs))
  where
    mem = vmMemory vm
    dynamicSize = staticBase (vmHeader vm)
    story = originalBytes mem
    diffs =
      [ peekByte mem i `xor` BS.index story i
      | i <- [0 .. dynamicSize - 1]
      ]
    dropTrailingZeros = reverse . dropWhile (== 0) . reverse
    encode [] = []
    encode xs@(0 : _) = 0 : fromIntegral (n - 1) : encode (drop n xs)
      where
        n = min 256 (length (takeWhile (== 0) xs))
    encode (b : rest) = b : encode rest

-- | Expand a CMem chunk over pristine memory.
applyCMem :: Memory -> Header -> ByteString -> Either String Memory
applyCMem mem0 hdr bytes = go mem0 0 (BS.unpack bytes)
  where
    dynamicSize = staticBase hdr
    go mem _ [] = Right mem
    go _ _ [0] = Left "Quetzal: CMem ends inside a zero run"
    go mem i (0 : n : rest) = go mem (i + 1 + fromIntegral n) rest
    go mem i (b : rest)
      | i >= dynamicSize = Left "Quetzal: CMem longer than dynamic memory"
      | otherwise = go (pokeByte i (b `xor` original i) mem) (i + 1) rest
    original i = BS.index (originalBytes mem0) i

-- | Copy a UMem chunk (a plain dump of dynamic memory) into place.
applyUMem :: Memory -> Header -> ByteString -> Either String Memory
applyUMem mem0 hdr bytes
  | BS.length bytes /= dynamicSize = Left "Quetzal: UMem has the wrong size"
  | otherwise = Right (foldr place mem0 [0 .. dynamicSize - 1])
  where
    dynamicSize = staticBase hdr
    place i mem
      | value /= peekByte mem i = pokeByte i value mem
      | otherwise = mem
      where
        value = BS.index bytes i

-- | The call stack, oldest frame first.  The base frame of the
-- machine doubles as Quetzal's required dummy frame.
stks :: VM -> ByteString
stks vm = BS.concat (map frameBytes (reverse (NE.toList (vmFrames vm))))

-- | One call frame in the Stks layout: return address, local count,
-- store variable, argument mask, evaluation stack depth, then the
-- locals and the pushed values.
frameBytes :: Frame -> ByteString
frameBytes f =
  BS.pack $
    w24 (frameReturnPC f)
      ++ [fromIntegral (Seq.length (frameLocals f))]
      ++ [frameStore f]
      ++ [argsMask (frameArgs f)]
      ++ w16 (length (frameEval f))
      ++ concatMap (w16 . fromIntegral) (frameLocals f)
      ++ concatMap (w16 . fromIntegral) (reverse (frameEval f))
  where
    argsMask n = (1 `shiftL` n) - 1

-- | Parse the frames of a Stks chunk, oldest first.
parseFrames :: ByteString -> Either String [Frame]
parseFrames bytes = go 0
  where
    go i
      | i == BS.length bytes = Right []
      | i + 8 > BS.length bytes = Left "Quetzal: truncated stack frame"
      | end > BS.length bytes = Left "Quetzal: truncated stack frame"
      | otherwise = (frame :) <$> go end
      where
        nLocals = fromIntegral (BS.index bytes (i + 3)) .&. 15
        nEval = fromIntegral (readW16 bytes (i + 6))
        end = i + 8 + 2 * (nLocals + nEval)
        word k = fromIntegral (readW16 bytes (i + 8 + 2 * k))
        frame =
          Frame
            { frameLocals = Seq.fromList (map word [0 .. nLocals - 1])
            , frameEval =
                reverse (map (word . (+ nLocals)) [0 .. nEval - 1])
            , frameReturnPC = readW24 bytes i
            , frameStore = BS.index bytes (i + 4)
            , frameArgs = popCount (BS.index bytes (i + 5))
            }

-- IFF plumbing

-- | Wrap chunks in a @FORM@ of type @IFZS@.
form :: [ByteString] -> ByteString
form parts =
  BS.concat (["FORM", BS.pack (w32 size), "IFZS"] ++ parts)
  where
    size = 4 + sum (map BS.length parts)

-- | A chunk with its identifier, length, and pad byte if odd.
chunk :: ByteString -> ByteString -> ByteString
chunk cid payload =
  BS.concat [cid, BS.pack (w32 (BS.length payload)), payload, pad]
  where
    pad = if odd (BS.length payload) then BS.singleton 0 else BS.empty

-- | The content of an @IFZS@ form, or an explanation of why not.
iffBody :: ByteString -> Either String ByteString
iffBody bs
  | BS.length bs < 12 = Left "Quetzal: file too short"
  | BS.take 4 bs /= "FORM" = Left "Quetzal: not an IFF file"
  | BS.take 4 (BS.drop 8 bs) /= "IFZS" = Left "Quetzal: not a saved game"
  | otherwise = Right (BS.take (size - 4) (BS.drop 12 bs))
  where
    size = fromIntegral (readW32 bs 4)

-- | Split a form body into (identifier, payload) sections.
chunksOf :: ByteString -> [(ByteString, ByteString)]
chunksOf bs = go 0
  where
    go i
      | i + 8 > BS.length bs = []
      | otherwise = (cid, payload) : go next
      where
        cid = BS.take 4 (BS.drop i bs)
        len = fromIntegral (readW32 bs (i + 4))
        payload = BS.take len (BS.drop (i + 8) bs)
        next = i + 8 + len + (len .&. 1)

w16 :: Int -> [Word8]
w16 n = [fromIntegral (n `shiftR` 8), fromIntegral n]

w24 :: Int -> [Word8]
w24 n = [fromIntegral (n `shiftR` 16), fromIntegral (n `shiftR` 8), fromIntegral n]

w32 :: Int -> [Word8]
w32 n = fromIntegral (n `shiftR` 24) : w24 n

readW16 :: ByteString -> Int -> Int
readW16 bs i = fromIntegral (BS.index bs i) `shiftL` 8 + fromIntegral (BS.index bs (i + 1))

readW24 :: ByteString -> Int -> Int
readW24 bs i = readW16 bs i `shiftL` 8 + fromIntegral (BS.index bs (i + 2))

readW32 :: ByteString -> Int -> Int
readW32 bs i = readW16 bs i `shiftL` 16 + readW16 bs (i + 2)
