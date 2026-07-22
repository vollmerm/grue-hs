-- | Decoding of Z-machine instructions.
--
-- An instruction is an opcode byte (giving the form, operand count and
-- opcode number), operand type information, the operands themselves,
-- and then, depending on the particular operation: a store variable, a
-- branch offset, and inline text.  This module decodes the version 3 and
-- 4 instruction sets into a typed representation; execution is left to
-- the interpreter.
module Grue.Instruction
  ( Op (..)
  , Operand (..)
  , Branch (..)
  , BranchDest (..)
  , Instruction (..)
  , decode
  , decodeBranch
  , storesResult
  , takesBranch
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Text (Text)
import Data.Word (Word16, Word8)
import Grue.Header
import Grue.Memory
import Grue.ZString

-- | The version 3 and 4 operations.
data Op
  = -- 2OP
    Je
  | Jl
  | Jg
  | DecChk
  | IncChk
  | Jin
  | Test
  | Or
  | And
  | TestAttr
  | SetAttr
  | ClearAttr
  | Store
  | InsertObj
  | Loadw
  | Loadb
  | GetProp
  | GetPropAddr
  | GetNextProp
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Call2s
  | -- 1OP
    Jz
  | GetSibling
  | GetChild
  | GetParent
  | GetPropLen
  | Inc
  | Dec
  | PrintAddr
  | RemoveObj
  | PrintObj
  | Ret
  | Jump
  | PrintPaddr
  | Load
  | Not
  | Call1s
  | -- 0OP
    Rtrue
  | Rfalse
  | Print
  | PrintRet
  | Nop
  | Save
  | Restore
  | Restart
  | RetPopped
  | Pop
  | Quit
  | NewLine
  | ShowStatus
  | Verify
  | -- VAR
    Call
  | Storew
  | Storeb
  | PutProp
  | Sread
  | PrintChar
  | PrintNum
  | Random
  | Push
  | Pull
  | SplitWindow
  | SetWindow
  | OutputStream
  | InputStream
  | SoundEffect
  | CallVs2
  | EraseWindow
  | EraseLine
  | SetCursor
  | GetCursor
  | SetTextStyle
  | BufferMode
  | ReadChar
  | ScanTable
  deriving (Eq, Show)

-- | A decoded operand.  Variable operands are resolved to values at
-- execution time, since reading variable 0 pops the stack.
data Operand
  = LargeConst Word16
  | SmallConst Word8
  | ByVariable Word8
  deriving (Eq, Show)

-- | Branch information: the truth value that causes the branch, where
-- the branch data itself lives (needed by save files), and where the
-- branch goes.
data Branch = Branch
  { branchWhen :: Bool
  , branchAt :: Int
  , branchDest :: BranchDest
  }
  deriving (Eq, Show)

-- | Branch offsets 0 and 1 mean return from the current routine;
-- anything else is an address computed at decode time.
data BranchDest
  = BranchReturnFalse
  | BranchReturnTrue
  | BranchAddr Int
  deriving (Eq, Show)

-- | A fully decoded instruction.
data Instruction = Instruction
  { instOp :: Op
  , instOperands :: [Operand]
  , instStore :: Maybe Word8
  , instBranch :: Maybe Branch
  , instText :: Maybe Text
  }
  deriving (Eq, Show)

-- | The number of operands an opcode form declares.
data OpCount = Count0 | Count1 | Count2 | CountVar
  deriving (Eq, Show)

-- | Look up an operation by version, operand count and opcode number.
-- Some opcode numbers only became meaningful in version 4.
lookupOp :: Int -> OpCount -> Int -> Maybe Op
lookupOp v Count2 n = case n of
  1 -> Just Je
  2 -> Just Jl
  3 -> Just Jg
  4 -> Just DecChk
  5 -> Just IncChk
  6 -> Just Jin
  7 -> Just Test
  8 -> Just Or
  9 -> Just And
  10 -> Just TestAttr
  11 -> Just SetAttr
  12 -> Just ClearAttr
  13 -> Just Store
  14 -> Just InsertObj
  15 -> Just Loadw
  16 -> Just Loadb
  17 -> Just GetProp
  18 -> Just GetPropAddr
  19 -> Just GetNextProp
  20 -> Just Add
  21 -> Just Sub
  22 -> Just Mul
  23 -> Just Div
  24 -> Just Mod
  25 | v >= 4 -> Just Call2s
  _ -> Nothing
lookupOp v Count1 n = case n of
  0 -> Just Jz
  1 -> Just GetSibling
  2 -> Just GetChild
  3 -> Just GetParent
  4 -> Just GetPropLen
  5 -> Just Inc
  6 -> Just Dec
  7 -> Just PrintAddr
  8 | v >= 4 -> Just Call1s
  9 -> Just RemoveObj
  10 -> Just PrintObj
  11 -> Just Ret
  12 -> Just Jump
  13 -> Just PrintPaddr
  14 -> Just Load
  15 -> Just Not
  _ -> Nothing
lookupOp _ Count0 n = case n of
  0 -> Just Rtrue
  1 -> Just Rfalse
  2 -> Just Print
  3 -> Just PrintRet
  4 -> Just Nop
  5 -> Just Save
  6 -> Just Restore
  7 -> Just Restart
  8 -> Just RetPopped
  9 -> Just Pop
  10 -> Just Quit
  11 -> Just NewLine
  12 -> Just ShowStatus
  13 -> Just Verify
  _ -> Nothing
lookupOp v CountVar n = case n of
  0 -> Just Call
  1 -> Just Storew
  2 -> Just Storeb
  3 -> Just PutProp
  4 -> Just Sread
  5 -> Just PrintChar
  6 -> Just PrintNum
  7 -> Just Random
  8 -> Just Push
  9 -> Just Pull
  10 -> Just SplitWindow
  11 -> Just SetWindow
  12 | v >= 4 -> Just CallVs2
  13 | v >= 4 -> Just EraseWindow
  14 | v >= 4 -> Just EraseLine
  15 | v >= 4 -> Just SetCursor
  16 | v >= 4 -> Just GetCursor
  17 | v >= 4 -> Just SetTextStyle
  18 | v >= 4 -> Just BufferMode
  19 -> Just OutputStream
  20 -> Just InputStream
  21 -> Just SoundEffect
  22 | v >= 4 -> Just ReadChar
  23 | v >= 4 -> Just ScanTable
  _ -> Nothing

-- | Whether an operation is followed by a store variable byte.
storesResult :: Op -> Bool
storesResult op =
  op
    `elem` [ Or
           , And
           , Loadw
           , Loadb
           , GetProp
           , GetPropAddr
           , GetNextProp
           , Add
           , Sub
           , Mul
           , Div
           , Mod
           , GetSibling
           , GetChild
           , GetParent
           , GetPropLen
           , Load
           , Not
           , Call
           , Call1s
           , Call2s
           , CallVs2
           , Random
           , ReadChar
           , ScanTable
           ]

-- | Whether an operation is followed by branch information.
takesBranch :: Op -> Bool
takesBranch op =
  op
    `elem` [ Je
           , Jl
           , Jg
           , DecChk
           , IncChk
           , Jin
           , Test
           , TestAttr
           , Jz
           , GetSibling
           , GetChild
           , Save
           , Restore
           , Verify
           , ScanTable
           ]

-- | Whether an operation is followed by inline text.
takesText :: Op -> Bool
takesText op = op == Print || op == PrintRet

-- | Decode the instruction at an address.  Returns the instruction and
-- the address of the next one.  An unknown opcode is an error: it
-- means the story is corrupt, or execution has jumped into data.
decode :: Memory -> Header -> Int -> (Instruction, Int)
decode mem hdr pc0 = (inst, pcText)
  where
    opByte = peekByte mem pc0

    (count, opNum, operandSpec, pcOperands) = case opByte of
      b
        | b >= 0xc0 ->
            -- Variable form: one type byte follows the opcode, or two
            -- for the "double variable" call, which takes up to eight
            -- operands.
            let num = fromIntegral (b .&. 31)
                double = testBit b 5 && num == 12
                types
                  | double =
                      typeBits (peekByte mem (pc0 + 1))
                        ++ typeBits (peekByte mem (pc0 + 2))
                  | otherwise = typeBits (peekByte mem (pc0 + 1))
             in ( if testBit b 5 then CountVar else Count2
                , num
                , takeWhile (/= 3) types
                , if double then pc0 + 3 else pc0 + 2
                )
        | b >= 0x80 ->
            -- Short form: bits 4 and 5 give the single operand's type
            -- (11 meaning no operand at all).
            case (b `shiftR` 4) .&. 3 of
              3 -> (Count0, fromIntegral (b .&. 15), [], pc0 + 1)
              t -> (Count1, fromIntegral (b .&. 15), [t], pc0 + 1)
        | otherwise ->
            -- Long form: always 2OP, with one type bit per operand
            -- (0 for a small constant, 1 for a variable).
            ( Count2
            , fromIntegral (b .&. 31)
            , [longType (testBit b 6), longType (testBit b 5)]
            , pc0 + 1
            )

    longType var = if var then 2 else 1

    op = case lookupOp (zVersion hdr) count opNum of
      Just o -> o
      Nothing ->
        error
          ( "Grue.Instruction.decode: unknown opcode "
              ++ show count
              ++ ":"
              ++ show opNum
              ++ " at address "
              ++ show pc0
          )

    (operands, pcStore) = readOperands operandSpec pcOperands

    (store, pcBranch)
      | storesResult op = (Just (peekByte mem pcStore), pcStore + 1)
      | otherwise = (Nothing, pcStore)

    (branch, pcAfterBranch)
      | takesBranch op =
          let (b, end) = decodeBranch mem pcBranch in (Just b, end)
      | otherwise = (Nothing, pcBranch)

    (text, pcText)
      | takesText op =
          let (t, end) = decodeString mem hdr pcAfterBranch
           in (Just t, end)
      | otherwise = (Nothing, pcAfterBranch)

    inst = Instruction op operands store branch text

    -- The four 2-bit type fields of a variable-form type byte, most
    -- significant first.  A type of 3 marks an omitted operand.
    typeBits tb = [fromIntegral (tb `shiftR` s) .&. 3 | s <- [6, 4, 2, 0]]

    readOperands [] pc = ([], pc)
    readOperands (t : ts) pc = (operand : rest, end)
      where
        (operand, pc') = case t of
          0 -> (LargeConst (peekWord mem pc), pc + 2)
          1 -> (SmallConst (peekByte mem pc), pc + 1)
          _ -> (ByVariable (peekByte mem pc), pc + 1)
        (rest, end) = readOperands ts pc'

-- | Read the one or two bytes of branch data at an address.  Returns
-- the branch and the address just past it.  This is also used on
-- restore, when execution resumes at the branch data of the original
-- @save@ instruction.
decodeBranch :: Memory -> Int -> (Branch, Int)
decodeBranch mem pc
  | testBit b1 6 = (branchFrom (pc + 1) (fromIntegral (b1 .&. 63)), pc + 1)
  | otherwise = (branchFrom (pc + 2) offset14, pc + 2)
  where
    b1 = peekByte mem pc
    raw :: Int
    raw =
      (fromIntegral (b1 .&. 63) `shiftL` 8)
        .|. fromIntegral (peekByte mem (pc + 1))
    offset14 = if raw >= 0x2000 then raw - 0x4000 else raw
    branchFrom after offset =
      Branch
        { branchWhen = testBit b1 7
        , branchAt = pc
        , branchDest = case offset of
            0 -> BranchReturnFalse
            1 -> BranchReturnTrue
            _ -> BranchAddr (after + offset - 2)
        }
