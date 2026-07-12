-- | The object table: attributes, the object tree, and properties.
--
-- Objects are numbered from 1, with 0 meaning \"nothing\".  This module
-- implements the version 1 to 3 layout: a 31-word property defaults
-- table, 9-byte object entries (32 attribute flags, byte-sized tree
-- links and a property table pointer), and property blocks with a
-- single size byte.
module Grue.Object
  ( -- * Attributes
    testAttr
  , setAttr
  , clearAttr

    -- * The object tree
  , parent
  , sibling
  , child
  , insertObject
  , removeObject

    -- * Properties
  , shortName
  , propertyValue
  , putProperty
  , propertyAddr
  , propertyLen
  , nextProperty

    -- * Whole-table inspection
  , objectCount
  ) where

import Data.Bits (clearBit, setBit, shiftR, testBit, (.&.))
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Grue.Header
import Grue.Memory
import Grue.ZString

-- | The byte address of an object's 9-byte entry.
objectAddr :: Header -> Int -> Int
objectAddr hdr obj = objectTableAddr hdr + 62 + 9 * (obj - 1)

-- | The byte address of an object's property table.
propTableAddr :: Memory -> Header -> Int -> Int
propTableAddr mem hdr obj = fromIntegral (peekWord mem (objectAddr hdr obj + 7))

-- | The byte within an object's entry holding an attribute, and the
-- bit position of the attribute in it.  Attributes are stored topmost
-- bit first: attribute 0 is bit 7 of the first byte.
attrLocation :: Header -> Int -> Int -> (Int, Int)
attrLocation hdr obj attr = (objectAddr hdr obj + attr `div` 8, 7 - attr `mod` 8)

-- | Test an attribute flag of an object.  Object 0 ("nothing") has no
-- attributes; queries about it are answered blandly rather than
-- treated as fatal, as reference interpreters do.
testAttr :: Memory -> Header -> Int -> Int -> Bool
testAttr _ _ 0 _ = False
testAttr mem hdr obj attr = testBit (peekByte mem addr) bit
  where
    (addr, bit) = attrLocation hdr obj attr

-- | Set an attribute flag of an object.
setAttr :: Header -> Int -> Int -> Memory -> Memory
setAttr _ 0 _ mem = mem
setAttr hdr obj attr mem = pokeByte addr (setBit (peekByte mem addr) bit) mem
  where
    (addr, bit) = attrLocation hdr obj attr

-- | Clear an attribute flag of an object.
clearAttr :: Header -> Int -> Int -> Memory -> Memory
clearAttr _ 0 _ mem = mem
clearAttr hdr obj attr mem = pokeByte addr (clearBit (peekByte mem addr) bit) mem
  where
    (addr, bit) = attrLocation hdr obj attr

-- | An object's parent (0 if none).
parent :: Memory -> Header -> Int -> Int
parent _ _ 0 = 0
parent mem hdr obj = fromIntegral (peekByte mem (objectAddr hdr obj + 4))

-- | An object's next sibling (0 if none).
sibling :: Memory -> Header -> Int -> Int
sibling _ _ 0 = 0
sibling mem hdr obj = fromIntegral (peekByte mem (objectAddr hdr obj + 5))

-- | An object's first child (0 if none).
child :: Memory -> Header -> Int -> Int
child _ _ 0 = 0
child mem hdr obj = fromIntegral (peekByte mem (objectAddr hdr obj + 6))

setParent, setSibling, setChild :: Header -> Int -> Int -> Memory -> Memory
setParent hdr obj v = pokeByte (objectAddr hdr obj + 4) (fromIntegral v)
setSibling hdr obj v = pokeByte (objectAddr hdr obj + 5) (fromIntegral v)
setChild hdr obj v = pokeByte (objectAddr hdr obj + 6) (fromIntegral v)

-- | Detach an object from the tree: unlink it from its parent's child
-- list and leave it parentless and siblingless.  Detaching an already
-- parentless object is a no-op.
removeObject :: Header -> Int -> Memory -> Memory
removeObject _ 0 mem = mem
removeObject hdr obj mem
  | p == 0 = mem
  | otherwise =
      setParent hdr obj 0 . setSibling hdr obj 0 . unlink $ mem
  where
    p = parent mem hdr obj
    next = sibling mem hdr obj
    unlink m
      | child m hdr p == obj = setChild hdr p next m
      | otherwise = relink (child m hdr p) m
    relink prev m
      | sibling m hdr prev == obj = setSibling hdr prev next m
      | otherwise = relink (sibling m hdr prev) m

-- | Move an object to become the first child of a destination object.
insertObject :: Header -> Int -> Int -> Memory -> Memory
insertObject _ 0 _ mem = mem
insertObject hdr obj dest mem =
  ( setChild hdr dest obj
      . setParent hdr obj dest
      . setSibling hdr obj (child detached hdr dest)
  )
    detached
  where
    detached = removeObject hdr obj mem

-- | An object's short name, decoded from the header of its property
-- table.  A zero-length name has no text stored at all.
shortName :: Memory -> Header -> Int -> Text
shortName _ _ 0 = T.empty
shortName mem hdr obj
  | peekByte mem base == 0 = T.empty
  | otherwise = decodeStringAt mem hdr (base + 1)
  where
    base = propTableAddr mem hdr obj

-- | One property of an object: its number, the address of its data,
-- and the data length in bytes.
data PropBlock = PropBlock
  { propNum :: Int
  , propDataAddr :: Int
  , propDataLen :: Int
  }

-- | The property blocks of an object, in the stored (descending
-- number) order.
propBlocks :: Memory -> Header -> Int -> [PropBlock]
propBlocks mem hdr obj = go firstProp
  where
    base = propTableAddr mem hdr obj
    nameWords = fromIntegral (peekByte mem base)
    firstProp = base + 1 + 2 * nameWords
    go addr = case fromIntegral (peekByte mem addr) :: Int of
      0 -> []
      size -> PropBlock num (addr + 1) len : go (addr + 1 + len)
        where
          num = size .&. 31
          len = size `shiftR` 5 + 1

-- | Find a property of an object by number.
findProp :: Memory -> Header -> Int -> Int -> Maybe PropBlock
findProp mem hdr obj n = find ((== n) . propNum) (propBlocks mem hdr obj)

-- | The value of a property: the object's own value if it provides
-- the property, and the entry from the property defaults table
-- otherwise.  One-byte properties read as their byte; longer ones as
-- the word at the start of their data.
propertyValue :: Memory -> Header -> Int -> Int -> Word16
propertyValue _ _ 0 _ = 0
propertyValue mem hdr obj n = case findProp mem hdr obj n of
  Nothing -> peekWord mem (objectTableAddr hdr + 2 * (n - 1))
  Just b
    | propDataLen b == 1 -> fromIntegral (peekByte mem (propDataAddr b))
    | otherwise -> peekWord mem (propDataAddr b)

-- | Write a property value.  A one-byte property stores the least
-- significant byte of the value.  The property must exist: writing a
-- property an object does not provide is a story bug.
putProperty :: Header -> Int -> Int -> Word16 -> Memory -> Memory
putProperty hdr obj n value mem = case findProp mem hdr obj n of
  Nothing ->
    error
      ( "Grue.Object.putProperty: object "
          ++ show obj
          ++ " has no property "
          ++ show n
      )
  Just b
    | propDataLen b == 1 -> pokeByte (propDataAddr b) (fromIntegral value) mem
    | otherwise -> pokeWord (propDataAddr b) value mem

-- | The byte address of a property's data, or 0 if the object does not
-- provide the property.
propertyAddr :: Memory -> Header -> Int -> Int -> Int
propertyAddr _ _ 0 _ = 0
propertyAddr mem hdr obj n = maybe 0 propDataAddr (findProp mem hdr obj n)

-- | The length of the property whose data starts at the given address,
-- read back from the size byte before it.  By convention an address of
-- 0 (an absent property) has length 0.
propertyLen :: Memory -> Int -> Int
propertyLen _ 0 = 0
propertyLen mem addr = fromIntegral (peekByte mem (addr - 1)) `shiftR` 5 + 1

-- | The number of the property listed after property @n@ of an object,
-- or the first property when @n@ is 0, or 0 when there are no more.
nextProperty :: Memory -> Header -> Int -> Int -> Int
nextProperty _ _ 0 _ = 0
nextProperty mem hdr obj n = case blocks of
  [] -> 0
  (b : _)
    | n == 0 -> propNum b
    | otherwise -> case dropWhile ((/= n) . propNum) blocks of
        (_ : after : _) -> propNum after
        _ -> 0
  where
    blocks = propBlocks mem hdr obj

-- | The number of objects in the table, deduced by the usual
-- convention that the object entries end where the first property
-- table begins.
objectCount :: Memory -> Header -> Int
objectCount mem hdr = go 1 maxBound
  where
    go obj lowestProp
      | objectAddr hdr obj >= lowestProp = obj - 1
      | otherwise = go (obj + 1) (min lowestProp (propTableAddr mem hdr obj))
