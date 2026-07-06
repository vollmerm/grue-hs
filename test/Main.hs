{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the grue-hs Z-machine interpreter.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Grue.Header
import Grue.Memory
import Grue.ZString
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain . tests =<< loadStories

tests :: [(FilePath, Memory)] -> TestTree
tests stories =
  testGroup
    "grue-hs"
    [memoryTests, headerTests, zstringTests, storyTests stories]

-- | Story files used for integration tests when available on this
-- machine.  Missing files are skipped silently, so the suite still
-- passes on machines without them.
storyPaths :: [FilePath]
storyPaths =
  [ "/Users/vollmerm/Repos/zifmia/zorks/zork1.z3"
  , "/Users/vollmerm/Repos/zifmia/zorks/minizork.z3"
  , "/Users/vollmerm/Repos/zifmia/advent/advent.z3"
  ]

loadStories :: IO [(FilePath, Memory)]
loadStories = fmap concat . mapM load $ storyPaths
  where
    load path = do
      exists <- doesFileExist path
      if exists
        then do
          bytes <- BS.readFile path
          pure [(path, fromStory bytes)]
        else pure []

-- | A little memory image with recognizable contents: byte @i@ holds
-- value @i@ for the first 256 bytes.
countingMemory :: Memory
countingMemory = fromStory (BS.pack [0 .. 255])

memoryTests :: TestTree
memoryTests =
  testGroup
    "Grue.Memory"
    [ testCase "memorySize matches story length" $
        memorySize countingMemory @?= 256
    , testCase "peekByte reads story bytes" $ do
        peekByte countingMemory 0 @?= 0
        peekByte countingMemory 17 @?= 17
        peekByte countingMemory 255 @?= 255
    , testCase "peekWord is big-endian" $
        peekWord countingMemory 1 @?= 0x0102
    , testCase "pokeByte overlays the story" $ do
        let mem = pokeByte 10 0xab countingMemory
        peekByte mem 10 @?= 0xab
        peekByte mem 9 @?= 9
        peekByte mem 11 @?= 11
    , testCase "pokeWord stores big-endian" $ do
        let mem = pokeWord 4 0xbeef countingMemory
        peekByte mem 4 @?= 0xbe
        peekByte mem 5 @?= 0xef
    , testCase "originalBytes ignores writes" $ do
        let mem = pokeWord 0 0xffff countingMemory
        originalBytes mem @?= BS.pack [0 .. 255]
    , testProperty "peekWord after pokeWord round-trips" $
        \(w :: Word) ->
          forAll (choose (0, 254)) $ \addr ->
            let value = fromIntegral w
                mem = pokeWord addr value countingMemory
             in peekWord mem addr === value
    , testProperty "peekByte after pokeByte round-trips" $
        \(w :: Word) ->
          forAll (choose (0, 255)) $ \addr ->
            let value = fromIntegral w
                mem = pokeByte addr value countingMemory
             in peekByte mem addr === value
    ]

-- | A synthetic version 3 story: a 64-byte header followed by the
-- given payload.  The abbreviations table address points at the start
-- of the payload.
v3Story :: [Word8] -> Memory
v3Story payload = fromStory (BS.pack (header ++ payload))
  where
    header =
      concatMap
        word
        [ (0x0300 :: Int) -- version 3, flags1 clear
        , 0, 0x4224 -- high memory base
        , 0x4321 -- initial program counter
        , 0x1234 -- dictionary
        , 0x0876 -- object table
        , 0x0102 -- globals
        , 0x0442 -- static memory base
        , 0, 0, 0, 0, 0x0040 -- abbreviations
        , 40 -- file length, stored divided by 2 in version 3
        , 0xbeef -- checksum
        ]
        ++ replicate 34 0
    -- 15 words of fields plus 34 zero bytes make the 64-byte header.
    word w = [fromIntegral (w `div` 256), fromIntegral (w `mod` 256)]

-- | The synthetic story used by the header tests, with a recognizable
-- 16-byte payload.
syntheticStory :: Memory
syntheticStory = v3Story (replicate 16 3)

-- | Split a 16-bit word into big-endian bytes.
wordBytes :: Word16 -> [Word8]
wordBytes w = [fromIntegral (w `div` 256), fromIntegral (w `mod` 256)]

headerTests :: TestTree
headerTests =
  testGroup
    "Grue.Header"
    [ testCase "reads fields of a synthetic story" $ do
        let hdr = readHeader syntheticStory
        zVersion hdr @?= 3
        highMemBase hdr @?= 0x4224
        initialPC hdr @?= 0x4321
        dictionaryAddr hdr @?= 0x1234
        objectTableAddr hdr @?= 0x0876
        globalsAddr hdr @?= 0x0102
        staticBase hdr @?= 0x0442
        abbreviationsAddr hdr @?= 0x0040
        fileLength hdr @?= 80
        checksum hdr @?= 0xbeef
    , testCase "packed addresses double in version 3" $ do
        let hdr = readHeader syntheticStory
        packedToByte hdr 0x2000 @?= 0x4000
    , testCase "computeChecksum sums bytes past the header" $ do
        let hdr = readHeader syntheticStory
        -- 16 payload bytes of value 3 each; declared length 80 caps the
        -- region at end of file.
        computeChecksum syntheticStory hdr @?= 48
    ]

-- | A story whose payload is the given Z-string words, placed at
-- address 64.
zstringStory :: [Word16] -> Memory
zstringStory = v3Story . concatMap wordBytes

-- | Decode the Z-string at address 64 of a story built from the given
-- words.
decodeWords :: [Word16] -> T.Text
decodeWords ws = decodeStringAt mem (readHeader mem) 64
  where
    mem = zstringStory ws

zstringTests :: TestTree
zstringTests =
  testGroup
    "Grue.ZString"
    [ testCase "decodes plain lower-case text" $
        -- "hello" is [13,10,17] [17,20,pad] with the end bit set.
        decodeWords [0x3551, 0xc685] @?= "hello"
    , testCase "returns the address past the end word" $ do
        let mem = zstringStory [0x3551, 0xc685]
        snd (decodeString mem (readHeader mem) 64) @?= 68
    , testCase "shifts select upper case and punctuation" $
        -- "Hi." is [4,13,14] [5,18,pad].
        decodeWords [0x11ae, 0x9645] @?= "Hi."
    , testCase "A2 character 7 is a new-line" $
        decodeWords [0x94e5] @?= "\n"
    , testCase "ZSCII escapes decode arbitrary characters" $
        -- "@" is code 64: [5,6,2] [0? no: continuation] — z-chars
        -- [5,6,2,0,5,5], where 2 and 0 are the halves of the code.
        decodeWords [0x14c2, 0x80a5] @?= "@"
    , testCase "abbreviations expand from the table" $ do
        -- Payload: entry 0 of the abbreviations table points at the
        -- "hello" string stored at byte 72 (word address 36), and the
        -- string at 66 is [1,0,pad]: abbreviation 0.
        let mem =
              v3Story . concat $
                [ wordBytes 36
                , concatMap wordBytes [0x8405]
                , [0, 0, 0, 0]
                , concatMap wordBytes [0x3551, 0xc685]
                ]
        decodeStringAt mem (readHeader mem) 66 @?= "hello"
    , testCase "the default Unicode table has 69 entries" $ do
        zsciiToChar 155 @?= Just 'ä'
        zsciiToChar 161 @?= Just 'ß'
        zsciiToChar 223 @?= Just '¿'
        zsciiToChar 224 @?= Nothing
    , testCase "encodeWord matches the standard's worked example" $ do
        let v4Header = (readHeader syntheticStory) {zVersion = 4}
        encodeWord v4Header "i" @?= [0x38a5, 0x14a5, 0x94a5]
    , testCase "encodeWord truncates to six Z-characters in version 3" $
        encodeWord (readHeader syntheticStory) "abcdefgh"
          @?= [0x18e8, 0xa54b]
    , testCase "encodeWord lower-cases its input" $ do
        let hdr = readHeader syntheticStory
        encodeWord hdr "Sword" @?= encodeWord hdr "sword"
    , testProperty "decode of encodeWord round-trips short words" $
        forAll (resize 6 (listOf1 (choose ('a', 'z')))) $ \letters ->
          let word = T.pack (take 6 letters)
           in decodeWords (encodeWord (readHeader syntheticStory) word)
                === word
    , testCase "punctuation survives the dictionary encoding" $
        decodeWords (encodeWord (readHeader syntheticStory) "x-ray")
          @?= "x-ray"
    ]

-- | Checks against real story files found on this machine.
storyTests :: [(FilePath, Memory)] -> TestTree
storyTests stories =
  testGroup "story files" (map storyTest stories)
  where
    storyTest (path, mem) =
      testGroup
        path
        [ testCase "is version 3" $
            zVersion (readHeader mem) @?= 3
        , testCase "declared length fits the file" $
            assertBool "file length exceeds actual size" $
              fileLength (readHeader mem) <= memorySize mem
        , testCase "checksum verifies" $
            assertBool "checksum mismatch" $
              checksumValid mem (readHeader mem)
        ]
