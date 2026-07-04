-- | Test suite for the grue-hs Z-machine interpreter.
module Main (main) where

import Data.ByteString qualified as BS
import Grue.Memory
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "grue-hs" [memoryTests]

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
