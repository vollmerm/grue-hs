{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the grue-hs Z-machine interpreter.
module Main (main) where

import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Grue.Dictionary
import Grue.Header
import Grue.Instruction
import Grue.Interp
import Grue.Memory
import Grue.Object qualified as Obj
import Grue.VM
import Grue.ZString
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
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
    , instructionTests
    , interpTests
    , storyTests stories
    ]

-- | A story file used for integration tests, with facts the tests
-- check against it.  One small, freely distributable story is bundled
-- with the repository; commercial story files are looked up in an
-- external collection (see 'storyRoot') and skipped when absent.
data StorySpec = StorySpec
  { specLocation :: StoryLocation
  , specKnownObjects :: [T.Text]
    -- ^ Object names that must appear in the object table.
  , specIntro :: [T.Text]
    -- ^ Substrings expected in the output before the first prompt.
    -- Empty also marks stories that open with a yes\/no question
    -- rather than a command prompt, which the save tests skip.
  , specMinWords :: Int
    -- ^ Sanity floor for the dictionary size.
  , specMinObjects :: Int
    -- ^ Sanity floor for the object count.
  }

-- | Bundled stories live at a path relative to the repository;
-- external ones are relative to the story collection.
data StoryLocation = Bundled FilePath | External FilePath

storySpecs :: [StorySpec]
storySpecs =
  [ StorySpec
      { specLocation = Bundled "test/stories/cloak.z3"
      , specKnownObjects = ["Foyer of the Opera House", "small brass hook"]
      , specIntro = ["Cloak of Darkness", "Foyer of the Opera House"]
      , specMinWords = 100
      , specMinObjects = 10
      }
  , StorySpec
      { specLocation = External "zorks/zork1.z3"
      , specKnownObjects = ["West of House", "brass lantern"]
      , specIntro = ["ZORK I", "West of House", "mailbox"]
      , specMinWords = 100
      , specMinObjects = 50
      }
  , StorySpec
      { specLocation = External "zorks/minizork.z3"
      , specKnownObjects = ["West of House"]
      , specIntro = ["West of House", "mailbox"]
      , specMinWords = 100
      , specMinObjects = 50
      }
  , StorySpec
      { specLocation = External "advent/advent.z3"
      , specKnownObjects = []
      , specIntro = []
      , specMinWords = 100
      , specMinObjects = 50
      }
  ]

-- | Where the external story collection lives: the @GRUE_STORY_DIR@
-- environment variable, or a @zifmia@ checkout beside this repository.
storyRoot :: IO FilePath
storyRoot = fromMaybe "../zifmia" <$> lookupEnv "GRUE_STORY_DIR"

-- | A story that was found and loaded.
data Story = Story
  { storyPath :: FilePath
  , storySpec :: StorySpec
  , storyMemory :: Memory
  }

loadStories :: IO [Story]
loadStories = do
  root <- storyRoot
  let resolve loc = case loc of
        Bundled path -> path
        External path -> root </> path
      load spec = do
        let path = resolve (specLocation spec)
        exists <- doesFileExist path
        if exists
          then do
            bytes <- BS.readFile path
            pure [Story path spec (fromStory bytes)]
          else pure []
  concat <$> mapM load storySpecs

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

-- | Decode the instruction assembled from the given bytes, placed at
-- address 64 of an otherwise empty story.
decodeAt :: [Word8] -> (Instruction, Int)
decodeAt bytes = decode mem (readHeader mem) 64
  where
    mem = mkStory [] bytes

instructionTests :: TestTree
instructionTests =
  testGroup
    "Grue.Instruction"
    [ testCase "long form with variable and small constant" $
        decodeAt [0x54, 0x10, 0x05, 0x00]
          @?= ( Instruction Add [ByVariable 16, SmallConst 5] (Just 0) Nothing Nothing
              , 68
              )
    , testCase "short form 1OP with a large constant and short branch" $
        decodeAt [0x80, 0x12, 0x34, 0xc5]
          @?= ( Instruction
                  Jz
                  [LargeConst 0x1234]
                  Nothing
                  (Just (Branch True 67 (BranchAddr 71)))
                  Nothing
              , 68
              )
    , testCase "branch offset 0 means return false" $
        decodeAt [0x90, 0x05, 0x40]
          @?= ( Instruction
                  Jz
                  [SmallConst 5]
                  Nothing
                  (Just (Branch False 66 BranchReturnFalse))
                  Nothing
              , 67
              )
    , testCase "long branches are 14-bit signed" $
        decodeAt [0x01, 0x01, 0x02, 0x3f, 0xff]
          @?= ( Instruction
                  Je
                  [SmallConst 1, SmallConst 2]
                  Nothing
                  (Just (Branch False 67 (BranchAddr 66)))
                  Nothing
              , 69
              )
    , testCase "variable form VAR with type byte" $
        decodeAt [0xe0, 0x2f, 0x12, 0x34, 0x07, 0x01]
          @?= ( Instruction
                  Call
                  [LargeConst 0x1234, ByVariable 7]
                  (Just 1)
                  Nothing
                  Nothing
              , 70
              )
    , testCase "variable form 2OP can carry three operands" $
        decodeAt [0xc1, 0x57, 1, 2, 3, 0xc5]
          @?= ( Instruction
                  Je
                  [SmallConst 1, SmallConst 2, SmallConst 3]
                  Nothing
                  (Just (Branch True 69 (BranchAddr 73)))
                  Nothing
              , 70
              )
    , testCase "print carries its inline text" $
        decodeAt [0xb2, 0x35, 0x51, 0xc6, 0x85]
          @?= ( Instruction Print [] Nothing Nothing (Just "hello")
              , 69
              )
    , testCase "get_child both stores and branches" $
        decodeAt [0xa2, 0x05, 0x00, 0x46]
          @?= ( Instruction
                  GetChild
                  [ByVariable 5]
                  (Just 0)
                  (Just (Branch False 67 (BranchAddr 72)))
                  Nothing
              , 68
              )
    ]

-- | Boot a story assembled from segments of bytes at absolute
-- addresses.  The header points the program counter at 64, the
-- dictionary at 0x100, and the globals at 0x130.
bootProg :: [(Int, [Word8])] -> VM
bootProg segments = boot flattened
  where
    mem0 = mkStory [(0x06, 64), (0x08, 0x100), (0x0c, 0x130)] (replicate 448 0)
    place (addr, bytes) m =
      foldr (\(i, b) -> pokeByte i b) m (zip [addr ..] bytes)
    mem = foldr place mem0 segments
    flattened = BS.pack [peekByte mem i | i <- [0 .. memorySize mem - 1]]

-- | Run a program consisting of a single code segment at address 64.
runProg :: [Word8] -> (T.Text, Stop)
runProg code = (out, stop)
  where
    (out, stop, _) = runProgVM code

-- | Like 'runProg', but also expose the final machine state.
runProgVM :: [Word8] -> (T.Text, Stop, VM)
runProgVM code = run (bootProg [(64, code)])

interpTests :: TestTree
interpTests =
  testGroup
    "Grue.Interp"
    [ testCase "adds and prints" $
        runProg [0x14, 3, 4, 0x00, 0xe6, 0xbf, 0x00, 0xba]
          @?= ("7", Halted)
    , testCase "calls a routine with default locals" $ do
        let main' = [0xe0, 0x3f, 0x00, 0x25, 0x10, 0xe6, 0xbf, 0x10, 0xba]
            routine = [0x01, 0x00, 0x05, 0xab, 0x01]
            (out, stop, _) = run (bootProg [(64, main'), (74, routine)])
        (out, stop) @?= ("5", Halted)
    , testCase "call arguments override default locals" $ do
        let main' = [0xe0, 0x1f, 0x00, 0x25, 0x09, 0x10, 0xe6, 0xbf, 0x10, 0xba]
            routine = [0x01, 0x00, 0x05, 0xab, 0x01]
            (out, stop, _) = run (bootProg [(64, main'), (74, routine)])
        (out, stop) @?= ("9", Halted)
    , testCase "a taken branch skips ahead" $
        runProg [0x03, 5, 3, 0xc5, 0xe6, 0x7f, 1, 0xe6, 0x7f, 2, 0xba]
          @?= ("2", Halted)
    , testCase "an untaken branch falls through" $
        runProg [0x02, 5, 3, 0xc5, 0xe6, 0x7f, 1, 0xe6, 0x7f, 2, 0xba]
          @?= ("12", Halted)
    , testCase "store and inc work on globals" $
        runProg [0x0d, 0x10, 0x01, 0x95, 0x10, 0xe6, 0xbf, 0x10, 0xba]
          @?= ("2", Halted)
    , testCase "push and pull use the evaluation stack" $
        runProg
          [ 0xe8, 0x7f, 7
          , 0xe8, 0x7f, 9
          , 0xe9, 0x7f, 0x10
          , 0xe6, 0xbf, 0x10
          , 0xe6, 0xbf, 0x00
          , 0xba
          ]
          @?= ("97", Halted)
    , testCase "random stays in range after seeding" $ do
        let (out, stop) =
              runProg
                [ 0xe7, 0x3f, 0xff, 0xfb, 0x00
                , 0xe7, 0x7f, 3, 0x10
                , 0xe6, 0xbf, 0x10
                , 0xba
                ]
        stop @?= Halted
        assertBool ("out of range: " ++ T.unpack out) $
          out `elem` ["1", "2", "3"]
    , testCase "output stream 3 redirects into memory" $ do
        -- Select a table at 0x180, print "hi" (redirected), deselect,
        -- then print "hi" again to the screen.
        let prog =
              [ 0xf3, 0x4f, 0x03, 0x01, 0x80
              , 0xb2, 0xb5, 0xc5
              , 0xf3, 0x3f, 0xff, 0xfd
              , 0xb2, 0xb5, 0xc5
              , 0xba
              ]
            (out, stop, vm) = run (bootProg [(64, prog)])
            mem = vmMemory vm
        (out, stop) @?= ("hi", Halted)
        peekWord mem 0x180 @?= 2
        peekByte mem 0x182 @?= fromIntegral (fromEnum 'h')
        peekByte mem 0x183 @?= fromIntegral (fromEnum 'i')
    , testCase "boot announces screen splitting in Flags 1" $ do
        let (_, _, vm) = runProgVM [0xba]
        assertBool "Flags 1 bit 5 clear" $
          testBit (peekByte (vmMemory vm) 0x01) 5
    , testCase "upper window text overlays and stays out of output" $ do
        -- Split off two rows, print "hi" up top, "hi" below, then
        -- reselect the upper window (cursor back to the top left) and
        -- overprint "X".
        let prog =
              [ 0xea, 0x7f, 2 -- split_window 2
              , 0xeb, 0x7f, 1 -- set_window 1
              , 0xb2, 0xb5, 0xc5 -- print "hi"
              , 0xeb, 0x7f, 0 -- set_window 0
              , 0xb2, 0xb5, 0xc5 -- print "hi"
              , 0xeb, 0x7f, 1 -- set_window 1
              , 0xb2, 0x93, 0xa5 -- print "X"
              , 0xba
              ]
            (out, stop, vm) = runProgVM prog
        (out, stop) @?= ("hi", Halted)
        toList (upperLines (vmUpper vm)) @?= ["Xi", ""]
    , testCase "the upper window never scrolls" $ do
        -- In a one-row window, "a" lands on the row and the text
        -- after the new-line falls off the bottom.
        let printANewlineB = [0xb2, 0x18, 0xa7, 0x9c, 0xa5] -- print "a^b"
            splitTo n =
              [0xea, 0x7f, n, 0xeb, 0x7f, 1] ++ printANewlineB ++ [0xba]
            (_, _, clipped) = runProgVM (splitTo 1)
            (_, _, roomy) = runProgVM (splitTo 2)
        toList (upperLines (vmUpper clipped)) @?= ["a"]
        toList (upperLines (vmUpper roomy)) @?= ["a", "b"]
    , testCase "splitting clears the upper window" $ do
        let prog =
              [ 0xea, 0x7f, 1 -- split_window 1
              , 0xeb, 0x7f, 1 -- set_window 1
              , 0xb2, 0xb5, 0xc5 -- print "hi"
              , 0xea, 0x7f, 1 -- split_window 1 again
              , 0xba
              ]
            (_, _, vm) = runProgVM prog
        toList (upperLines (vmUpper vm)) @?= [""]
    , testCase "read fills the text and parse buffers" $ do
        let v3hdr = readHeader (mkStory [] [])
            entry w = concatMap wordBytes (encodeWord v3hdr w) ++ [0, 0, 0]
            dict =
              [3, 46, 44, 34, 7, 0, 4]
                ++ concatMap entry ["go", "look", "nearby", "sword"]
            prog = [0xe4, 0x0f, 0x01, 0x80, 0x01, 0xc0, 0xba]
            vm0 =
              bootProg
                [(64, prog), (0x100, dict), (0x180, [20]), (0x1c0, [5])]
            (out1, stop1, vm1) = run vm0
        (out1, stop1) @?= ("", NeedInput)
        let vm2 = provideInput "go  EAST" vm1
            (_, stop2, vm3) = run vm2
            mem = vmMemory vm3
        stop2 @?= Halted
        -- The text buffer holds the lower-cased line, zero-terminated.
        [peekByte mem (0x181 + i) | i <- [0 .. 8]]
          @?= map (fromIntegral . fromEnum) "go  east\NUL"
        -- Two words: "go" found in the dictionary, "east" not.
        peekByte mem 0x1c1 @?= 2
        peekWord mem 0x1c2 @?= 0x0107
        peekByte mem 0x1c4 @?= 2
        peekByte mem 0x1c5 @?= 1
        peekWord mem 0x1c6 @?= 0
        peekByte mem 0x1c8 @?= 4
        peekByte mem 0x1c9 @?= 5
    ]

-- | Checks against real story files found on this machine.
storyTests :: [Story] -> TestTree
storyTests stories =
  testGroup "story files" (map storyTest stories)
  where
    storyTest story =
      testGroup (storyPath story) (basicTests ++ saveTests)
      where
       spec = storySpec story
       mem = storyMemory story
       known = specKnownObjects spec
       intro = specIntro spec
       basicTests =
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
              dictEntryCount dict >= specMinWords spec
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
            assertBool
              "suspiciously few objects"
              (count >= specMinObjects spec)
            assertBool "orphaned object" (all wellPlaced [1 .. count])
        , testCase "known object names appear" $ do
            let hdr = readHeader mem
                count = Obj.objectCount mem hdr
                names = map (Obj.shortName mem hdr) [1 .. count]
            sequence_
              [ assertBool (T.unpack name ++ " missing") (name `elem` names)
              | name <- known
              ]
        , testCase "boots and runs to the first prompt" $ do
            let (out, stop, _) = run (boot (originalBytes mem))
            stop @?= NeedInput
            assertBool "no output before the prompt" (not (T.null out))
            sequence_
              [ assertBool (T.unpack s ++ " missing") (s `T.isInfixOf` out)
              | s <- intro
              ]
        ]
       -- Stories that open with a yes/no question (rather than a
       -- normal prompt) would misread these scripted commands.
       saveTests
        | null intro = []
        | otherwise =
        [ testCase "a saved game restores in a fresh machine" $ do
            -- Play a move, save, and capture the Quetzal bytes.
            let (_, _, vm1) = run (boot (originalBytes mem))
                (_, s2, vm3) = run (provideInput "north" vm1)
            s2 @?= NeedInput
            let (_, s3, vm5) = run (provideInput "save" vm3)
            bytes <- case s3 of
              SaveRequested b -> pure b
              other -> assertFailure ("expected a save request: " ++ show other)
            -- Continue after a successful save...
            let (outA, sA, vmA) = run (finishSave True vm5)
            -- ...and separately, restore the bytes in a fresh machine.
            let (_, _, f1) = run (boot (originalBytes mem))
                (_, sR, f3) = run (provideInput "restore" f1)
            sR @?= RestoreRequested
            let (outB, sB, vmB) = run (finishRestore (Just bytes) f3)
            (outB, sB) @?= (outA, sA)
            vmPC vmB @?= vmPC vmA
            vmFrames vmB @?= vmFrames vmA
            let dynamic vm =
                  [ peekByte (vmMemory vm) i
                  | i <- [0x40 .. staticBase (vmHeader vm) - 1]
                  ]
            dynamic vmB @?= dynamic vmA
        , testCase "restoring garbage reports failure to the story" $ do
            let (_, _, vm1) = run (boot (originalBytes mem))
                (_, sR, vm2) = run (provideInput "restore" vm1)
            sR @?= RestoreRequested
            let (out, stop, _) = run (finishRestore (Just "not a save") vm2)
            stop @?= NeedInput
            assertBool "story kept running" (not (T.null out))
        ]
