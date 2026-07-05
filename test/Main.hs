-- | Test suite for the grue-hs Z-machine interpreter.
module Main (main) where

import Data.ByteString qualified as BS
import Grue.Header
import Grue.Memory
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain . tests =<< loadStories

tests :: [(FilePath, Memory)] -> TestTree
tests stories = testGroup "grue-hs" [memoryTests, headerTests, storyTests stories]

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

-- | A synthetic version 3 story: a 64-byte header followed by a little
-- payload, with distinctive values in each field read by 'readHeader'.
syntheticStory :: Memory
syntheticStory = fromStory (BS.pack (header ++ payload))
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
    payload = replicate 16 3
    -- 15 words of fields plus 34 zero bytes make the 64-byte header.
    word w = [fromIntegral (w `div` 256), fromIntegral (w `mod` 256)]

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
