-- | The object table: attributes, the object tree, and properties.
--
-- Objects are numbered from 1, with 0 meaning \"nothing\".  The table
-- layout depends on the story version.  Versions 1 to 3 use a 31-word
-- property defaults table and 9-byte entries (32 attribute flags,
-- byte-sized tree links and a property-table pointer), with property
-- blocks introduced by a single size byte.  Versions 4 and later use a
-- 63-word defaults table and 14-byte entries (48 attribute flags,
-- word-sized tree links), with property blocks introduced by one or two
-- size bytes.
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

-- | The version-dependent shape of the object table.
data Layout = Layout
  { entrySize :: Int
  -- ^ Bytes per object entry: 9 in versions 1 to 3, 14 from version 4.
  , defaultsBytes :: Int
  -- ^ Size of the property defaults table (31 or 63 words).
  , attrBytes :: Int
  -- ^ Bytes of attribute flags (4 or 6) at the start of an entry.
  , linkWord :: Bool
  -- ^ Whether tree links are 2-byte words (version 4) or single bytes.
  , propPtrOffset :: Int
  -- ^ Offset within an entry of the property-table pointer word.
  }

-- | The object-table layout for a story version.
layout :: Header -> Layout
layout hdr
  | zVersion hdr <= 3 = Layout 9 62 4 False 7
  | otherwise = Layout 14 126 6 True 12

-- | The byte address of an object's entry.
objectAddr :: Header -> Int -> Int
objectAddr hdr obj =
  objectTableAddr hdr + defaultsBytes l + entrySize l * (obj - 1)
  where
    l = layout hdr

-- | The byte address of an object's property table.
propTableAddr :: Memory -> Header -> Int -> Int
propTableAddr mem hdr obj =
  fromIntegral (peekWord mem (objectAddr hdr obj + propPtrOffset (layout hdr)))

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

-- | The byte address of tree link @k@ (0 parent, 1 sibling, 2 child)
-- within an object's entry.
linkAddr :: Header -> Int -> Int -> Int
linkAddr hdr obj k = objectAddr hdr obj + attrBytes l + k * linkSize
  where
    l = layout hdr
    linkSize = if linkWord l then 2 else 1

-- | Read one of an object's tree links, respecting the version's link
-- width.
readLink :: Memory -> Header -> Int -> Int -> Int
readLink mem hdr obj k
  | linkWord (layout hdr) = fromIntegral (peekWord mem addr)
  | otherwise = fromIntegral (peekByte mem addr)
  where
    addr = linkAddr hdr obj k

-- | Write one of an object's tree links.
writeLink :: Header -> Int -> Int -> Int -> Memory -> Memory
writeLink hdr obj k v
  | linkWord (layout hdr) = pokeWord addr (fromIntegral v)
  | otherwise = pokeByte addr (fromIntegral v)
  where
    addr = linkAddr hdr obj k

-- | An object's parent (0 if none).
parent :: Memory -> Header -> Int -> Int
parent _ _ 0 = 0
parent mem hdr obj = readLink mem hdr obj 0

-- | An object's next sibling (0 if none).
sibling :: Memory -> Header -> Int -> Int
sibling _ _ 0 = 0
sibling mem hdr obj = readLink mem hdr obj 1

-- | An object's first child (0 if none).
child :: Memory -> Header -> Int -> Int
child _ _ 0 = 0
child mem hdr obj = readLink mem hdr obj 2

setParent, setSibling, setChild :: Header -> Int -> Int -> Memory -> Memory
setParent hdr obj v = writeLink hdr obj 0 v
setSibling hdr obj v = writeLink hdr obj 1 v
setChild hdr obj v = writeLink hdr obj 2 v

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
      size -> PropBlock num dataAddr len : go (dataAddr + len)
        where
          (num, len, dataAddr) = decodeSize mem hdr addr size

-- | Decode a property's size byte(s) at an address into its number,
-- data length and the address of its data.  Versions 1 to 3 pack both
-- into a single byte; versions 4 and later use the low six bits for the
-- number and, when the top bit is set, a second byte for the length.
decodeSize :: Memory -> Header -> Int -> Int -> (Int, Int, Int)
decodeSize mem hdr addr size
  | zVersion hdr <= 3 = (size .&. 31, size `shiftR` 5 + 1, addr + 1)
  | testBit size 7 = (size .&. 63, longLen, addr + 2)
  | testBit size 6 = (size .&. 63, 2, addr + 1)
  | otherwise = (size .&. 63, 1, addr + 1)
  where
    longLen = case fromIntegral (peekByte mem (addr + 1)) .&. 63 of
      0 -> 64
      n -> n

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
propertyLen :: Memory -> Header -> Int -> Int
propertyLen _ _ 0 = 0
propertyLen mem hdr addr
  | zVersion hdr <= 3 = fromIntegral b `shiftR` 5 + 1
  | testBit b 7 = case fromIntegral b .&. 63 of 0 -> 64; n -> n
  | testBit b 6 = 2
  | otherwise = 1
  where
    b = peekByte mem (addr - 1)

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
