{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the grue-hs Z-machine interpreter.
module Main (main) where

import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Grue.Dictionary
import Grue.Header
import Grue.Memory
import Grue.Object qualified as Obj
import Grue.ZString
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain . tests =<< loadStories

tests :: [Story] -> TestTree
tests stories =
  testGroup
    "grue-hs"
    [ memoryTests
    , headerTests
    , zstringTests
    , dictionaryTests
    , objectTests
    , storyTests stories
    ]

-- | Story files used for integration tests when available on this
-- machine, along with object names known to appear in them.  Missing
-- files are skipped silently, so the suite still passes on machines
-- without them.
data Story = Story
  { storyPath :: FilePath
  , storyKnownObjects :: [T.Text]
  , storyMemory :: Memory
  }

storyPaths :: [(FilePath, [T.Text])]
storyPaths =
  [ ( "/Users/vollmerm/Repos/zifmia/zorks/zork1.z3"
    , ["West of House", "brass lantern"]
    )
  , ("/Users/vollmerm/Repos/zifmia/zorks/minizork.z3", ["West of House"])
  , ("/Users/vollmerm/Repos/zifmia/advent/advent.z3", [])
  ]

loadStories :: IO [Story]
loadStories = fmap concat . mapM load $ storyPaths
  where
    load (path, known) = do
      exists <- doesFileExist path
      if exists
        then do
          bytes <- BS.readFile path
          pure [Story path known (fromStory bytes)]
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

-- | Build a synthetic version 3 story: a zeroed 64-byte header with
-- the given word fields poked in, followed by a payload starting at
-- address 64.
mkStory :: [(Int, Word16)] -> [Word8] -> Memory
mkStory fields payload = foldr (uncurry pokeWord) versioned fields
  where
    versioned = pokeByte 0 3 blank
    blank = fromStory (BS.pack (replicate 64 0 ++ payload))

-- | The synthetic story used by the header tests, with distinctive
-- values in every field read by 'readHeader'.
syntheticStory :: Memory
syntheticStory =
  mkStory
    [ (0x04, 0x4224) -- high memory base
    , (0x06, 0x4321) -- initial program counter
    , (0x08, 0x1234) -- dictionary
    , (0x0a, 0x0876) -- object table
    , (0x0c, 0x0102) -- globals
    , (0x0e, 0x0442) -- static memory base
    , (0x18, 0x0040) -- abbreviations
    , (0x1a, 40) -- file length, stored divided by 2 in version 3
    , (0x1c, 0xbeef) -- checksum
    ]
    (replicate 16 3)

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
-- address 64, with the abbreviations table also rooted there.
zstringStory :: [Word16] -> Memory
zstringStory = mkStory [(0x18, 64)] . concatMap wordBytes

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
              mkStory [(0x18, 64)] . concat $
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

-- | A story holding a small dictionary at address 64: separators
-- @. , \"@, entry length 7, and four sorted words.
dictStory :: Memory
dictStory = mkStory [(0x08, 64)] payload
  where
    v3hdr = readHeader (mkStory [] [])
    entry w = concatMap wordBytes (encodeWord v3hdr w) ++ [0, 0, 0]
    payload =
      [3, 46, 44, 34, 7, 0, 4]
        ++ concatMap entry ["go", "look", "nearby", "sword"]

dictionaryTests :: TestTree
dictionaryTests =
  testGroup
    "Grue.Dictionary"
    [ testCase "reads the dictionary header" $ do
        let dict = readDictionary dictStory (readHeader dictStory)
        dictSeparators dict @?= ".,\""
        dictEntryLength dict @?= 7
        dictEntryCount dict @?= 4
        dictEntriesAddr dict @?= 71
    , testCase "finds every word at its entry address" $ do
        let hdr = readHeader dictStory
            dict = readDictionary dictStory hdr
        lookupWord dictStory hdr dict "go" @?= Just 71
        lookupWord dictStory hdr dict "look" @?= Just 78
        lookupWord dictStory hdr dict "nearby" @?= Just 85
        lookupWord dictStory hdr dict "sword" @?= Just 92
    , testCase "misses absent words" $ do
        let hdr = readHeader dictStory
            dict = readDictionary dictStory hdr
        lookupWord dictStory hdr dict "xyzzy" @?= Nothing
    , testCase "long words match by their truncation" $ do
        let hdr = readHeader dictStory
            dict = readDictionary dictStory hdr
        lookupWord dictStory hdr dict "nearbyish" @?= Just 85
    , testCase "tokenize follows the standard's example" $ do
        let dict = readDictionary dictStory (readHeader dictStory)
        tokenize dict "fred,go  fishing"
          @?= [(0, "fred"), (4, ","), (5, "go"), (9, "fishing")]
    , testCase "tokenize of empty input is empty" $ do
        let dict = readDictionary dictStory (readHeader dictStory)
        tokenize dict "   " @?= []
    ]

-- | A story with a three-object tree at address 64: object 1 ("box")
-- has children 2 and 3.  Object 1 provides properties 18 (word) and
-- 4 (byte); the defaults table gives property 5 the value 0xab.
objStory :: Memory
objStory = mkStory [(0x0a, 64), (72, 0x00ab)] payload
  where
    defaults = replicate 62 0
    entry (par, sib, chi, props) =
      [0, 0, 0, 0, par, sib, chi] ++ wordBytes props
    entries =
      concatMap
        entry
        [ (0, 0, 2, 153) -- object 1
        , (1, 3, 0, 162) -- object 2
        , (1, 0, 0, 167) -- object 3
        ]
    propTable1 =
      [1, 0x9e, 0x9d] -- short name "box" in one word
        ++ [50, 0xca, 0xfe] -- property 18, two bytes
        ++ [4, 7] -- property 4, one byte
        ++ [0]
    propTable2 = [0] ++ [36, 0x12, 0x34] ++ [0]
    propTable3 = [0, 0]
    payload = defaults ++ entries ++ propTable1 ++ propTable2 ++ propTable3

objectTests :: TestTree
objectTests =
  testGroup
    "Grue.Object"
    [ testCase "reads tree links" $ do
        let hdr = readHeader objStory
        Obj.parent objStory hdr 2 @?= 1
        Obj.sibling objStory hdr 2 @?= 3
        Obj.child objStory hdr 1 @?= 2
        Obj.parent objStory hdr 1 @?= 0
    , testCase "attributes set, test, and clear" $ do
        let hdr = readHeader objStory
        Obj.testAttr objStory hdr 1 0 @?= False
        let mem1 = Obj.setAttr hdr 1 0 objStory
        Obj.testAttr mem1 hdr 1 0 @?= True
        peekByte mem1 (64 + 62) @?= 0x80
        let mem2 = Obj.setAttr hdr 1 31 mem1
        Obj.testAttr mem2 hdr 1 31 @?= True
        peekByte mem2 (64 + 62 + 3) @?= 0x01
        let mem3 = Obj.clearAttr hdr 1 0 mem2
        Obj.testAttr mem3 hdr 1 0 @?= False
        Obj.testAttr mem3 hdr 1 31 @?= True
    , testCase "reads short names" $ do
        let hdr = readHeader objStory
        Obj.shortName objStory hdr 1 @?= "box"
        Obj.shortName objStory hdr 2 @?= ""
    , testCase "property values, defaults, and writes" $ do
        let hdr = readHeader objStory
        Obj.propertyValue objStory hdr 1 18 @?= 0xcafe
        Obj.propertyValue objStory hdr 1 4 @?= 7
        Obj.propertyValue objStory hdr 1 5 @?= 0x00ab
        Obj.propertyValue objStory hdr 1 1 @?= 0
        let mem = Obj.putProperty hdr 1 4 0xff12 objStory
        Obj.propertyValue mem hdr 1 4 @?= 0x12
        let mem2 = Obj.putProperty hdr 1 18 0x5555 objStory
        Obj.propertyValue mem2 hdr 1 18 @?= 0x5555
    , testCase "property addresses and lengths" $ do
        let hdr = readHeader objStory
        Obj.propertyAddr objStory hdr 1 18 @?= 157
        Obj.propertyAddr objStory hdr 1 4 @?= 160
        Obj.propertyAddr objStory hdr 1 5 @?= 0
        Obj.propertyLen objStory 157 @?= 2
        Obj.propertyLen objStory 160 @?= 1
        Obj.propertyLen objStory 0 @?= 0
    , testCase "walks the property list in order" $ do
        let hdr = readHeader objStory
        Obj.nextProperty objStory hdr 1 0 @?= 18
        Obj.nextProperty objStory hdr 1 18 @?= 4
        Obj.nextProperty objStory hdr 1 4 @?= 0
        Obj.nextProperty objStory hdr 3 0 @?= 0
    , testCase "removeObject unlinks a first child" $ do
        let hdr = readHeader objStory
            mem = Obj.removeObject hdr 2 objStory
        Obj.child mem hdr 1 @?= 3
        Obj.parent mem hdr 2 @?= 0
        Obj.sibling mem hdr 2 @?= 0
    , testCase "removeObject unlinks a later sibling" $ do
        let hdr = readHeader objStory
            mem = Obj.removeObject hdr 3 objStory
        Obj.child mem hdr 1 @?= 2
        Obj.sibling mem hdr 2 @?= 0
        Obj.parent mem hdr 3 @?= 0
    , testCase "insertObject makes the first child" $ do
        let hdr = readHeader objStory
            mem = Obj.insertObject hdr 3 2 objStory
        Obj.child mem hdr 2 @?= 3
        Obj.parent mem hdr 3 @?= 2
        Obj.sibling mem hdr 3 @?= 0
        Obj.child mem hdr 1 @?= 2
        Obj.sibling mem hdr 2 @?= 0
    , testCase "counts the objects" $
        Obj.objectCount objStory (readHeader objStory) @?= 3
    ]

-- | Checks against real story files found on this machine.
storyTests :: [Story] -> TestTree
storyTests stories =
  testGroup "story files" (map storyTest stories)
  where
    storyTest (Story path known mem) =
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
        , testCase "dictionary words all look up" $ do
            let hdr = readHeader mem
                dict = readDictionary mem hdr
                words' = allWords mem hdr dict
            assertBool "suspiciously small dictionary" $
              dictEntryCount dict > 100
            -- Entries containing spaces are legal but deliberately
            -- unmatchable (their padding differs from typed input), so
            -- only space-free words are expected to round-trip.
            let misses =
                  [ w
                  | w <- words'
                  , not (T.any (== ' ') w)
                  , isNothing (lookupWord mem hdr dict w)
                  ]
            take 5 misses @?= []
        , testCase "object tree is well-founded" $ do
            let hdr = readHeader mem
                count = Obj.objectCount mem hdr
                childrenOf o =
                  takeWhile (/= 0) $
                    iterate (Obj.sibling mem hdr) (Obj.child mem hdr o)
                wellPlaced o =
                  Obj.parent mem hdr o == 0
                    || o `elem` childrenOf (Obj.parent mem hdr o)
            assertBool "suspiciously few objects" (count > 50)
            assertBool "orphaned object" (all wellPlaced [1 .. count])
        , testCase "known object names appear" $ do
            let hdr = readHeader mem
                count = Obj.objectCount mem hdr
                names = map (Obj.shortName mem hdr) [1 .. count]
            sequence_
              [ assertBool (T.unpack name ++ " missing") (name `elem` names)
              | name <- known
              ]
        ]
