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
  , bootWithSeed

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

    -- * The transcript (output stream 2)
  , transcriptOn
  , setTranscript
  , emitTranscript
  , takeTranscript

    -- * The upper window and screen state
  , UpperWindow (..)
  , splitUpper
  , selectWindow
  , writeUpper
  , setCursor
  , cursorPosition
  , eraseWindow
  , eraseLine
  , setTextStyle
  , setBufferMode

    -- * Sound
  , emitBeep
  , takeBeeps

    -- * Random numbers
  , Rng
  , seededRng
  , nextRandom
  ) where

import Data.Bits (clearBit, setBit, shiftR, testBit, xor, (.|.))
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
      -- | Byte address of the text buffer.
      Int
      -- | Byte address of the parse buffer.
      Int
  | -- | A single keypress for @read_char@; the store variable receives
    -- the ZSCII code.
    PendingReadChar Word8
  | PendingSave Branch
  | PendingRestore Branch
  deriving (Eq, Show)

-- | The upper window: a fixed region at the top of the screen the story
-- draws on after selecting it with @set_window@.  In version 3 it sits
-- below the interpreter's status line; from version 4 the game draws its
-- own status region here instead.  Printing overlays whatever is already
-- there, and the window never scrolls.
data UpperWindow = UpperWindow
  { upperHeight :: Int
  -- ^ How many screen rows the window occupies.
  , upperCursor :: (Int, Int)
  -- ^ Where the next character lands: row and column, zero-based.
  , upperLines :: Seq Text
  -- ^ The window's rows, exactly 'upperHeight' of them, stored
  -- unpadded: columns beyond a line's end are blank.
  }
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
  , vmTranscript :: [Text]
  -- ^ Buffered game-transcript text (output stream 2), most recent
  -- chunk first, for the frontend to write somewhere durable.
  , vmWindow :: Int
  -- ^ The window receiving output: 0 for the scrolling lower
  -- window, 1 for the upper.
  , vmUpper :: UpperWindow
  , vmTextStyle :: Int
  -- ^ The active text style, a bitmask of reverse video (1), bold (2),
  -- italic (4) and fixed pitch (8), as set by @set_text_style@.
  , vmBufferMode :: Bool
  -- ^ Whether lower-window output is buffered for word-wrapping
  -- (@buffer_mode@); on by default.
  , vmBeeps :: Int
  -- ^ Bleeps requested by @sound_effect@ and not yet sounded.
  , vmTables :: [(Int, Int)]
  -- ^ Active memory output streams (stream 3), innermost first:
  -- the table's byte address and the number of characters written
  -- so far.  While any is active, output goes there instead of the
  -- screen.
  , vmPending :: Maybe PendingInput
  }
  deriving (Eq, Show)

-- | Build the initial machine state from story file bytes, seeding the
-- random number generator with a fixed value so that runs are
-- reproducible.
boot :: ByteString -> VM
boot = bootWithSeed 0x2a

-- | Build the initial machine state with an explicit random seed.
-- Frontends pass a seed drawn from the environment for genuine
-- unpredictability during play; the fixed-seed 'boot' keeps tests and
-- transcripts deterministic.
bootWithSeed :: Word64 -> ByteString -> VM
bootWithSeed seed story =
  VM
    { vmMemory = mem
    , vmHeader = hdr
    , vmPC = initialPC hdr
    , vmFrames = baseFrame :| []
    , vmRng = seededRng seed
    , vmOutput = []
    , vmTranscript = []
    , vmWindow = 0
    , vmUpper = UpperWindow 0 (0, 0) Seq.empty
    , vmTextStyle = 0
    , vmBufferMode = True
    , vmBeeps = 0
    , vmTables = []
    , vmPending = Nothing
    }
  where
    loaded = fromStory story
    mem
      | peekByte loaded 0x00 <= 3 =
          -- Bit 5 of Flags 1 announces that screen splitting is
          -- available.
          pokeByte 0x01 (peekByte loaded 0x01 .|. 0x20) loaded
      | otherwise = stampCapabilities loaded
    hdr = readHeader mem
    baseFrame = Frame Seq.empty [] 0 0 0

-- | Announce version 4 display capabilities in the header and record
-- the interpreter's identity and screen size.  Flags 1 advertises only
-- a fixed-space font (bit 4): the frontends print the upper window in a
-- fixed-pitch font but do not render the bold, italic or reverse text
-- styles, and colours, pictures, sound and timed input are likewise not
-- provided.
stampCapabilities :: Memory -> Memory
stampCapabilities =
  pokeByte 0x01 0x10
    . pokeByte 0x1e 6 -- interpreter number: IBM PC
    . pokeByte 0x1f 0x41 -- interpreter version: 'A'
    . pokeByte 0x20 25 -- screen height in lines
    . pokeByte 0x21 80 -- screen width in characters

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

-- | Whether the game transcript (output stream 2) is running: bit 0
-- of Flags 2, which the story may also set or clear directly.
transcriptOn :: VM -> Bool
transcriptOn vm = testBit (peekWord (vmMemory vm) 0x10) 0

-- | Turn the game transcript on or off.  The standard requires the
-- @output_stream@ opcode to keep this Flags 2 bit in step.
setTranscript :: Bool -> VM -> VM
setTranscript on vm =
  vm {vmMemory = pokeWord 0x10 (adjust (peekWord (vmMemory vm) 0x10)) (vmMemory vm)}
  where
    adjust = if on then (`setBit` 0) else (`clearBit` 0)

-- | Append a chunk of transcript text.
emitTranscript :: Text -> VM -> VM
emitTranscript t vm = vm {vmTranscript = t : vmTranscript vm}

-- | Remove and return all buffered transcript text, oldest first.
takeTranscript :: VM -> (Text, VM)
takeTranscript vm =
  (T.concat (reverse (vmTranscript vm)), vm {vmTranscript = []})

-- | Give the upper window a new height.  In version 3 a screen split
-- always clears the window to blanks; from version 4 the existing
-- contents are kept, with rows added or dropped to match the new
-- height, and the cursor left where it is unless the window has shrunk
-- past it.
splitUpper :: Int -> VM -> VM
splitUpper n vm
  | zVersion (vmHeader vm) <= 3 =
      vm {vmUpper = UpperWindow n (0, 0) (Seq.replicate n T.empty)}
  | otherwise =
      vm {vmUpper = UpperWindow n cursor (fit n (upperLines old))}
  where
    old = vmUpper vm
    (row, col) = upperCursor old
    cursor = if row < n then (row, col) else (0, 0)
    fit h ls = Seq.take h (ls <> Seq.replicate h T.empty)

-- | Select the window that receives output.  Whenever the upper
-- window is selected, its cursor moves to the top left.
selectWindow :: Int -> VM -> VM
selectWindow 1 vm =
  vm {vmWindow = 1, vmUpper = (vmUpper vm) {upperCursor = (0, 0)}}
selectWindow _ vm = vm {vmWindow = 0}

-- | Print text into the upper window at its cursor.  Characters
-- overlay whatever is already on the row; a new-line moves to the
-- start of the next row.  The window never scrolls, so anything
-- printed below the bottom row is discarded.
writeUpper :: Text -> VM -> VM
writeUpper t vm = vm {vmUpper = go (vmUpper vm) t}
  where
    go w s =
      let (chunk, rest) = T.break (== '\n') s
       in case T.uncons rest of
            Nothing -> overlay chunk w
            Just (_, more) -> go (advance (overlay chunk w)) more
    advance w = w {upperCursor = (fst (upperCursor w) + 1, 0)}
    overlay chunk w@(UpperWindow height (row, col) ls)
      | T.null chunk || row >= height = w
      | otherwise =
          w
            { upperLines = Seq.adjust' place row ls
            , upperCursor = (row, col + T.length chunk)
            }
      where
        place line =
          let padded = line <> T.replicate (col - T.length line) (T.singleton ' ')
           in T.take col padded <> chunk <> T.drop (col + T.length chunk) padded

-- | Move the upper window's cursor.  Rows and columns are given
-- one-based, as the @set_cursor@ opcode supplies them.
setCursor :: Int -> Int -> VM -> VM
setCursor row col vm =
  vm {vmUpper = (vmUpper vm) {upperCursor = (max 0 (row - 1), max 0 (col - 1))}}

-- | The upper window's cursor, one-based (row, column), as reported by
-- @get_cursor@.
cursorPosition :: VM -> (Int, Int)
cursorPosition vm = (row + 1, col + 1)
  where
    (row, col) = upperCursor (vmUpper vm)

-- | Clear a window, or collapse the split for the whole-screen forms.
-- Clearing the lower window (0) is left to the frontend; window 1 and
-- the whole-screen form (-2) blank the upper window, while -1 also
-- collapses the split.
eraseWindow :: Int -> VM -> VM
eraseWindow w vm = case w of
  1 -> blankUpper vm
  (-2) -> blankUpper vm
  (-1) -> selectWindow 0 (splitUpper 0 vm)
  _ -> vm
  where
    blankUpper v =
      let h = upperHeight (vmUpper v)
       in v {vmUpper = UpperWindow h (0, 0) (Seq.replicate h T.empty)}

-- | Erase from the upper window's cursor to the end of its current row.
-- Stored rows are unpadded, so truncating to the cursor column blanks
-- the remainder.
eraseLine :: VM -> VM
eraseLine vm
  | row < upperHeight w =
      vm {vmUpper = w {upperLines = Seq.adjust' (T.take col) row (upperLines w)}}
  | otherwise = vm
  where
    w = vmUpper vm
    (row, col) = upperCursor w

-- | Set the text style: a bitmask combining reverse video, bold, italic
-- and fixed pitch.  Style 0 (roman) clears all of them; others
-- accumulate.
setTextStyle :: Int -> VM -> VM
setTextStyle 0 vm = vm {vmTextStyle = 0}
setTextStyle s vm = vm {vmTextStyle = vmTextStyle vm .|. s}

-- | Turn word-wrap buffering of the lower window on or off.
setBufferMode :: Bool -> VM -> VM
setBufferMode on vm = vm {vmBufferMode = on}

-- | Ask the frontend for a bleep.
emitBeep :: VM -> VM
emitBeep vm = vm {vmBeeps = vmBeeps vm + 1}

-- | Remove and return the number of pending bleeps.
takeBeeps :: VM -> (Int, VM)
takeBeeps vm = (vmBeeps vm, vm {vmBeeps = 0})

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
