-- | The state of a running Z-machine.
--
-- The machine consists of story memory, a program counter, a stack of
-- routine call frames, and a random number generator.  Output text
-- accumulates in a buffer that the frontend flushes; input is
-- requested by pausing execution (see "Grue.Interp").
--
-- All state is immutable: each operation produces a new 'VM'.
module Grue.VM
  ( VM (..)
  , Frame (..)
  , PendingInput (..)
  , boot

    -- * Variables
  , readVar
  , writeVar
  , peekVar
  , pokeVar

    -- * The current frame
  , pushEval
  , popEval

    -- * Output
  , emit
  , takeOutput

    -- * Random numbers
  , Rng (..)
  , seededRng
  , nextRandom
  ) where

import Data.Bits (shiftR, xor)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word64, Word8)
import Grue.Header
import Grue.Instruction (Branch)
import Grue.Memory

-- | One routine activation.
data Frame = Frame
  { frameLocals :: Seq Word16
    -- ^ The routine's local variables (up to 15).
  , frameEval :: [Word16]
    -- ^ This routine's portion of the evaluation stack, topmost first.
  , frameReturnPC :: Int
    -- ^ Where execution resumes after this routine returns.
  , frameStore :: Word8
    -- ^ The variable that receives this routine's return value.
  , frameArgs :: Int
    -- ^ How many arguments the call supplied, recorded for save files.
  }
  deriving (Eq, Show)

-- | A paused request that the frontend must complete: a line of
-- player input, or the file transfer for a save or restore.  The save
-- and restore cases remember the instruction's branch so the outcome
-- can be reported to the story.
data PendingInput
  = PendingRead
      { pendingTextBuffer :: Int
      , pendingParseBuffer :: Int
      }
  | PendingSave Branch
  | PendingRestore Branch
  deriving (Eq, Show)

-- | The complete machine state.
data VM = VM
  { vmMemory :: Memory
  , vmHeader :: Header
  , vmPC :: Int
  , vmFrames :: NonEmpty Frame
    -- ^ Call frames, current routine first.  The base frame is the
    -- story's top-level execution, which never returns.
  , vmRng :: Rng
  , vmOutput :: [Text]
    -- ^ Buffered output, most recent chunk first.
  , vmTables :: [(Int, Int)]
    -- ^ Active memory output streams (stream 3), innermost first:
    -- the table's byte address and the number of characters written
    -- so far.  While any is active, output goes there instead of the
    -- screen.
  , vmPending :: Maybe PendingInput
  }
  deriving (Eq, Show)

-- | Build the initial machine state from story file bytes.
boot :: ByteString -> VM
boot story =
  VM
    { vmMemory = mem
    , vmHeader = hdr
    , vmPC = initialPC hdr
    , vmFrames = baseFrame :| []
    , vmRng = seededRng 0x2a
    , vmOutput = []
    , vmTables = []
    , vmPending = Nothing
    }
  where
    mem = fromStory story
    hdr = readHeader mem
    baseFrame = Frame Seq.empty [] 0 0 0

-- | Apply a function to the current (topmost) frame.
onFrame :: (Frame -> Frame) -> VM -> VM
onFrame f vm =
  vm {vmFrames = f (NE.head frames) :| NE.tail frames}
  where
    frames = vmFrames vm

-- | Push a value on the evaluation stack.
pushEval :: Word16 -> VM -> VM
pushEval v = onFrame (\f -> f {frameEval = v : frameEval f})

-- | Pop the top of the evaluation stack.  Popping an empty stack is a
-- story bug.
popEval :: VM -> (Word16, VM)
popEval vm = case frameEval (NE.head (vmFrames vm)) of
  [] -> error "Grue.VM.popEval: evaluation stack underflow"
  (v : rest) -> (v, onFrame (\f -> f {frameEval = rest}) vm)

-- | Read a variable by number: 0 pops the stack, 1 to 15 are locals of
-- the current routine, 16 to 255 are globals.
readVar :: Word8 -> VM -> (Word16, VM)
readVar 0 vm = popEval vm
readVar n vm = (peekVar n vm, vm)

-- | Write a variable by number: 0 pushes on the stack.
writeVar :: Word8 -> Word16 -> VM -> VM
writeVar 0 v vm = pushEval v vm
writeVar n v vm = pokeVar n v vm

-- | Read a variable without consuming stack: 0 reads the top of the
-- stack in place.  This is the behaviour required for indirect
-- variable references (@inc@, @load@ and friends).
peekVar :: Word8 -> VM -> Word16
peekVar n vm
  | n == 0 = case frameEval (NE.head (vmFrames vm)) of
      [] -> error "Grue.VM.peekVar: evaluation stack underflow"
      (v : _) -> v
  | n <= 15 = localVar
  | otherwise = peekWord (vmMemory vm) (globalAddr vm n)
  where
    locals = frameLocals (NE.head (vmFrames vm))
    localVar = case Seq.lookup (fromIntegral n - 1) locals of
      Just v -> v
      Nothing ->
        error ("Grue.VM.peekVar: no local variable " ++ show n)

-- | Write a variable without growing the stack: 0 replaces the top of
-- the stack in place, as indirect variable references require.
pokeVar :: Word8 -> Word16 -> VM -> VM
pokeVar n v vm
  | n == 0 = case frameEval (NE.head (vmFrames vm)) of
      [] -> error "Grue.VM.pokeVar: evaluation stack underflow"
      (_ : rest) -> onFrame (\f -> f {frameEval = v : rest}) vm
  | n <= 15 =
      onFrame
        (\f -> f {frameLocals = Seq.update (fromIntegral n - 1) v (frameLocals f)})
        vm
  | otherwise =
      vm {vmMemory = pokeWord (globalAddr vm n) v (vmMemory vm)}

-- | The byte address of a global variable (numbers 16 to 255).
globalAddr :: VM -> Word8 -> Int
globalAddr vm n = globalsAddr (vmHeader vm) + 2 * (fromIntegral n - 16)

-- | Append a chunk of output text.
emit :: Text -> VM -> VM
emit t vm = vm {vmOutput = t : vmOutput vm}

-- | Remove and return all buffered output, oldest first.
takeOutput :: VM -> (Text, VM)
takeOutput vm = (T.concat (reverse (vmOutput vm)), vm {vmOutput = []})

-- | A small splitmix-style pseudo-random number generator.  The
-- Z-machine only needs uniform values in small ranges, and keeping the
-- generator local avoids an external dependency.
newtype Rng = Rng Word64
  deriving (Eq, Show)

-- | A generator seeded from the given value.
seededRng :: Word64 -> Rng
seededRng = Rng

-- | A uniformly distributed value in @[1, range]@, with the advanced
-- generator.
nextRandom :: Int -> Rng -> (Int, Rng)
nextRandom range (Rng s0) = (1 + fromIntegral (z3 `mod` fromIntegral range), Rng s1)
  where
    s1 = s0 + 0x9e3779b97f4a7c15
    z1 = (s1 `xor` (s1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
    z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    z3 = z2 `xor` (z2 `shiftR` 31)
