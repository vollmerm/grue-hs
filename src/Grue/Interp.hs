{-# LANGUAGE OverloadedStrings #-}

-- | Instruction execution: the heart of the interpreter.
--
-- Execution is pure.  'run' advances the machine until it either needs
-- a line of player input or halts, accumulating output text along the
-- way; the frontend prints the text, gathers input, and resumes with
-- 'provideInput'.
module Grue.Interp
  ( Stop (..)
  , run
  , provideInput
  , finishSave
  , finishRestore

    -- * The status line
  , StatusLine (..)
  , RightStatus (..)
  , statusLine
  ) where

import Control.Monad (void)
import Control.Monad.State
import Data.Bits (complement, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Int (Int16)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Grue.Dictionary
import Grue.Header
import Grue.Instruction
import Grue.Memory
import Grue.Object qualified as Obj
import Grue.Quetzal
import Grue.VM
import Grue.ZString

-- | Why execution paused.
data Stop
  = NeedInput
    -- ^ The @read@ opcode wants a line of input; resume with
    -- 'provideInput' followed by 'run'.
  | SaveRequested ByteString
    -- ^ The story wants these bytes written somewhere durable; report
    -- the outcome with 'finishSave' and 'run' again.
  | RestoreRequested
    -- ^ The story wants a previously saved game back; supply the file
    -- with 'finishRestore' and 'run' again.
  | Halted
    -- ^ The story has ended.
  deriving (Eq, Show)

-- | A computation over machine state.
type Z = State VM

-- | Run until input is needed or the story quits.  Returns the output
-- accumulated since the last flush.
run :: VM -> (Text, Stop, VM)
run vm0 = case runState step vm0 of
  (Nothing, vm) -> run vm
  (Just stop, vm) ->
    let (out, vm') = takeOutput vm in (out, stop, vm')

-- | Decode and execute one instruction.
step :: Z (Maybe Stop)
step = do
  vm <- get
  let (inst, next) = decode (vmMemory vm) (vmHeader vm) (vmPC vm)
  put vm {vmPC = next}
  exec inst

-- | The value of an operand.  Reading a variable operand may pop the
-- evaluation stack, so operands are evaluated left to right.
value :: Operand -> Z Word16
value (LargeConst w) = pure w
value (SmallConst b) = pure (fromIntegral b)
value (ByVariable v) = state (readVar v)

values :: [Operand] -> Z [Word16]
values = traverse value

-- | Store a result if the instruction has a store variable.
storeTo :: Maybe Word8 -> Word16 -> Z ()
storeTo Nothing _ = pure ()
storeTo (Just v) w = modify (writeVar v w)

-- | Act on a branch: if the condition matches the branch sense, either
-- jump or return from the current routine.
branchOn :: Maybe Branch -> Bool -> Z ()
branchOn Nothing _ = pure ()
branchOn (Just b) cond
  | cond /= branchWhen b = pure ()
  | otherwise = case branchDest b of
      BranchReturnFalse -> returnValue 0
      BranchReturnTrue -> returnValue 1
      BranchAddr a -> modify (\vm -> vm {vmPC = a})

-- | Return from the current routine, storing the result where the
-- call asked for it.
returnValue :: Word16 -> Z ()
returnValue w = do
  vm <- get
  case vmFrames vm of
    _ :| [] -> error "Grue.Interp: return from the top level"
    f :| (g : rest) -> do
      put vm {vmFrames = g :| rest, vmPC = frameReturnPC f}
      modify (writeVar (frameStore f) w)

-- | Call a routine.  Calling address 0 is legal and simply produces
-- the result 0.
callRoutine :: Word16 -> [Word16] -> Maybe Word8 -> Z ()
callRoutine 0 _ st = storeTo st 0
callRoutine packed args st = do
  vm <- get
  let mem = vmMemory vm
      hdr = vmHeader vm
      addr = packedToByte hdr packed
      count = fromIntegral (peekByte mem addr)
      initials = [peekWord mem (addr + 1 + 2 * i) | i <- [0 .. count - 1]]
      locals = zipWith fromMaybe initials (map Just args ++ repeat Nothing)
      frame =
        Frame
          { frameLocals = Seq.fromList locals
          , frameEval = []
          , frameReturnPC = vmPC vm
          , frameStore = fromMaybe 0 st
          , frameArgs = length args
          }
  put
    vm
      { vmFrames = NE.cons frame (vmFrames vm)
      , vmPC = addr + 1 + 2 * count
      }

-- | Interpret a machine word as signed.
signed :: Word16 -> Int
signed = fromIntegral . fromIntegral @Word16 @Int16

-- | Truncate a signed value back to a machine word.
unsigned :: Int -> Word16
unsigned = fromIntegral

-- | Emit text: to the selected window normally (lower-window text
-- also reaches any running transcript), or into the story's memory
-- table while a stream 3 redirection is active (each character lands
-- as a ZSCII byte, new-lines as code 13).
output :: Text -> Z ()
output t = do
  vm <- get
  case vmTables vm of
    []
      | vmWindow vm == 1 -> put (writeUpper t vm)
      | otherwise ->
          put (emit t (if transcriptOn vm then emitTranscript t vm else vm))
    (table, count) : rest -> do
      let codes = mapMaybe charToZscii (T.unpack t)
          place i code = pokeByte (table + 2 + count + i) (fromIntegral code)
          mem = foldr (uncurry place) (vmMemory vm) (zip [0 ..] codes)
      put vm {vmMemory = mem, vmTables = (table, count + length codes) : rest}

-- | Execute one decoded instruction.
exec :: Instruction -> Z (Maybe Stop)
exec (Instruction op operands st br text) = case op of
  -- Comparisons and jumps
  Je -> branchy $ \vs -> case vs of
    (a : others) -> pure (a `elem` others)
    [] -> badOperands
  Jl -> branch2 $ \a b -> pure (signed a < signed b)
  Jg -> branch2 $ \a b -> pure (signed a > signed b)
  Jz -> branch1 $ \a -> pure (a == 0)
  Jin -> branch2 $ \a b ->
    withWorld (\mem hdr -> Obj.parent mem hdr (obj a) == obj b)
  Test -> branch2 $ \bitmap flags -> pure (bitmap .&. flags == flags)
  TestAttr -> branch2 $ \o a ->
    withWorld (\mem hdr -> Obj.testAttr mem hdr (obj o) (fromIntegral a))
  DecChk -> continue $ do
    (ref, limit) <- val2
    v <- adjustVar ref (subtract 1)
    branchOn br (signed v < signed limit)
  IncChk -> continue $ do
    (ref, limit) <- val2
    v <- adjustVar ref (+ 1)
    branchOn br (signed v > signed limit)
  Jump -> continue $ do
    offset <- val1
    modify (\vm -> vm {vmPC = vmPC vm + signed offset - 2})
  -- Arithmetic and logic
  Add -> arith (+)
  Sub -> arith (-)
  Mul -> arith (*)
  Div -> arithSigned quot
  Mod -> arithSigned rem
  Or -> arith (.|.)
  And -> arith (.&.)
  Not -> continue $ do
    a <- val1
    storeTo st (complement a)
  -- Variables
  Store -> continue $ do
    (ref, v) <- val2
    modify (pokeVar (fromIntegral ref) v)
  Load -> continue $ do
    ref <- val1
    v <- gets (peekVar (fromIntegral ref))
    storeTo st v
  Inc -> continue $ do
    ref <- val1
    void (adjustVar ref (+ 1))
  Dec -> continue $ do
    ref <- val1
    void (adjustVar ref (subtract 1))
  Push -> continue $ do
    v <- val1
    modify (pushEval v)
  Pop -> continue (void (state popEval))
  Pull -> continue $ do
    ref <- val1
    v <- state popEval
    modify (pokeVar (fromIntegral ref) v)
  -- Memory access
  Loadw -> continue $ do
    (base, idx) <- val2
    v <- gets (\vm -> peekWord (vmMemory vm) (arrayAddr base (2 * idx)))
    storeTo st v
  Loadb -> continue $ do
    (base, idx) <- val2
    v <- gets (\vm -> peekByte (vmMemory vm) (arrayAddr base idx))
    storeTo st (fromIntegral v)
  Storew -> continue $ do
    (base, idx, v) <- val3
    onMemory (pokeWord (arrayAddr base (2 * idx)) v)
  Storeb -> continue $ do
    (base, idx, v) <- val3
    onMemory (pokeByte (arrayAddr base idx) (fromIntegral v))
  -- Objects
  GetParent -> continue $ do
    o <- val1
    v <- withWorld (\mem hdr -> Obj.parent mem hdr (obj o))
    storeTo st (fromIntegral v)
  GetSibling -> treeStep Obj.sibling
  GetChild -> treeStep Obj.child
  SetAttr -> continue $ do
    (o, a) <- val2
    onWorld (\hdr -> Obj.setAttr hdr (obj o) (fromIntegral a))
  ClearAttr -> continue $ do
    (o, a) <- val2
    onWorld (\hdr -> Obj.clearAttr hdr (obj o) (fromIntegral a))
  InsertObj -> continue $ do
    (o, dest) <- val2
    onWorld (\hdr -> Obj.insertObject hdr (obj o) (obj dest))
  RemoveObj -> continue $ do
    o <- val1
    onWorld (\hdr -> Obj.removeObject hdr (obj o))
  GetProp -> continue $ do
    (o, p) <- val2
    v <- withWorld (\mem hdr -> Obj.propertyValue mem hdr (obj o) (fromIntegral p))
    storeTo st v
  GetPropAddr -> continue $ do
    (o, p) <- val2
    v <- withWorld (\mem hdr -> Obj.propertyAddr mem hdr (obj o) (fromIntegral p))
    storeTo st (fromIntegral v)
  GetNextProp -> continue $ do
    (o, p) <- val2
    v <- withWorld (\mem hdr -> Obj.nextProperty mem hdr (obj o) (fromIntegral p))
    storeTo st (fromIntegral v)
  GetPropLen -> continue $ do
    addr <- val1
    v <- gets (\vm -> Obj.propertyLen (vmMemory vm) (fromIntegral addr))
    storeTo st (fromIntegral v)
  PutProp -> continue $ do
    (o, p, v) <- val3
    onWorld (\hdr -> Obj.putProperty hdr (obj o) (fromIntegral p) v)
  -- Printing
  Print -> continue $ output (fromMaybe T.empty text)
  PrintRet -> continue $ do
    output (fromMaybe T.empty text <> "\n")
    returnValue 1
  NewLine -> continue $ output "\n"
  PrintChar -> continue $ do
    code <- val1
    output (maybe T.empty T.singleton (zsciiToChar code))
  PrintNum -> continue $ do
    n <- val1
    output (T.pack (show (signed n)))
  PrintAddr -> continue $ do
    addr <- val1
    t <- withWorld (\mem hdr -> decodeStringAt mem hdr (fromIntegral addr))
    output t
  PrintPaddr -> continue $ do
    paddr <- val1
    t <- withWorld
      (\mem hdr -> decodeStringAt mem hdr (packedToByte hdr paddr))
    output t
  PrintObj -> continue $ do
    o <- val1
    t <- withWorld (\mem hdr -> Obj.shortName mem hdr (obj o))
    output t
  -- Control
  Call -> continue $ do
    vals <- values operands
    case vals of
      (routine : args) -> callRoutine routine args st
      [] -> error "Grue.Interp: call with no operands"
  Ret -> continue $ do
    v <- val1
    returnValue v
  Rtrue -> continue (returnValue 1)
  Rfalse -> continue (returnValue 0)
  RetPopped -> continue (state popEval >>= returnValue)
  Nop -> continue (pure ())
  -- Randomness
  Random -> continue $ do
    range <- val1
    case compare (signed range) 0 of
      GT -> do
        v <- state $ \vm ->
          let (v, rng) = nextRandom (signed range) (vmRng vm)
           in (v, vm {vmRng = rng})
        storeTo st (unsigned v)
      LT -> do
        modify (\vm -> vm {vmRng = seededRng (fromIntegral (negate (signed range)))})
        storeTo st 0
      EQ -> do
        modify (\vm -> vm {vmRng = advance (vmRng vm)})
        storeTo st 0
  -- Input
  Sread -> do
    (tbuf, pbuf) <- val2
    modify $ \vm ->
      vm {vmPending = Just (PendingRead (fromIntegral tbuf) (fromIntegral pbuf))}
    pure (Just NeedInput)
  -- The wider world
  Verify -> branch0 $
    gets (\vm -> checksumValid (vmMemory vm) (vmHeader vm))
  Quit -> pure (Just Halted)
  Restart -> continue $ modify restart
  Save -> case br of
    Nothing -> continue (pure ())
    Just b -> do
      vm <- get
      put vm {vmPending = Just (PendingSave b)}
      pure (Just (SaveRequested (saveState vm (branchAt b))))
  Restore -> case br of
    Nothing -> continue (pure ())
    Just b -> do
      modify (\vm -> vm {vmPending = Just (PendingRestore b)})
      pure (Just RestoreRequested)
  -- Display control
  ShowStatus -> continue (pure ())
  SplitWindow -> continue (val1 >>= modify . splitUpper . fromIntegral)
  SetWindow -> continue (val1 >>= modify . selectWindow . fromIntegral)
  OutputStream -> continue $ do
    vals <- values operands
    case map signed vals of
      (2 : _) -> modify (setTranscript True)
      (3 : table : _) -> modify $ \vm ->
        vm {vmTables = (table, 0) : vmTables vm}
      (-2) : _ -> modify (setTranscript False)
      (-3) : _ -> modify closeTable
      _ -> pure ()
  InputStream -> continue (void (values operands))
  SoundEffect -> continue $ do
    vals <- values operands
    case vals of
      (n : _) | n > 2 -> pure () -- sampled sounds are not provided
      _ -> modify emitBeep
  where
    continue act = Nothing <$ act

    val1 = values operands >>= expect1
    val2 = values operands >>= expect2
    val3 = values operands >>= expect3
    expect1 vs = case vs of
      [a] -> pure a
      _ -> badOperands
    expect2 vs = case vs of
      [a, b] -> pure (a, b)
      _ -> badOperands
    expect3 vs = case vs of
      [a, b, c] -> pure (a, b, c)
      _ -> badOperands
    badOperands =
      error ("Grue.Interp: wrong operand count for " ++ show op)

    branchy cond = continue (values operands >>= cond >>= branchOn br)
    branch0 cond = continue (cond >>= branchOn br)
    branch1 cond = continue (val1 >>= cond >>= branchOn br)
    branch2 cond = continue (val2 >>= uncurry cond >>= branchOn br)

    arith f = continue $ do
      (a, b) <- val2
      storeTo st (f a b)

    arithSigned f = continue $ do
      (a, b) <- val2
      if b == 0
        then error "Grue.Interp: division by zero"
        else storeTo st (unsigned (signed a `f` signed b))

    obj = fromIntegral @Word16 @Int

    arrayAddr base offset = fromIntegral (base + offset)

    withWorld f = gets (\vm -> f (vmMemory vm) (vmHeader vm))

    onMemory f = modify (\vm -> vm {vmMemory = f (vmMemory vm)})

    onWorld f = modify (\vm -> vm {vmMemory = f (vmHeader vm) (vmMemory vm)})

    -- get_sibling and get_child both store their result and branch on
    -- it being non-zero.
    treeStep link = continue $ do
      o <- val1
      v <- withWorld (\mem hdr -> link mem hdr (obj o))
      storeTo st (fromIntegral v)
      branchOn br (v /= 0)

    -- Adjust a variable in place through an indirect reference,
    -- returning the new value.
    adjustVar ref f = state $ \vm ->
      let n = fromIntegral ref
          v = unsigned (f (signed (peekVar n vm)))
       in (v, pokeVar n v vm)

    advance rng = snd (nextRandom 1 rng)

-- | Report whether the requested save was written.  The story sees
-- the outcome through the @save@ instruction's branch.
finishSave :: Bool -> VM -> VM
finishSave ok vm = case vmPending vm of
  Just (PendingSave b) ->
    execState (branchOn (Just b) ok) vm {vmPending = Nothing}
  _ -> error "Grue.Interp.finishSave: no save is pending"

-- | Complete a requested restore with the bytes of a save file (or
-- 'Nothing' if none could be read).  On success the machine resumes
-- from the moment of the original save, with its branch taken as
-- true; on any failure the @restore@ instruction's branch reports it.
finishRestore :: Maybe ByteString -> VM -> VM
finishRestore mbytes vm = case vmPending vm of
  Just (PendingRestore b) ->
    case attempt =<< mbytes of
      Just restored -> restored
      Nothing -> execState (branchOn (Just b) False) failed
    where
      failed = vm {vmPending = Nothing}
      attempt bytes = case restoreState story bytes of
        Left _ -> Nothing
        Right fresh -> Just (resume fresh)
      story = originalBytes (vmMemory vm)
      resume fresh = execState (branchOn (Just b') True) prepared
        where
          (b', after) = decodeBranch (vmMemory fresh) (vmPC fresh)
          -- The transcript and fixed-pitch bits of Flags 2 survive a
          -- restore, as the standard requires; output and randomness
          -- carry over from the running machine.
          flags2 =
            peekWord (vmMemory fresh) 0x10 .&. complement 0x3
              .|. (peekWord (vmMemory vm) 0x10 .&. 0x3)
          prepared =
            fresh
              { vmPC = after
              , vmMemory = pokeWord 0x10 flags2 (vmMemory fresh)
              , vmOutput = vmOutput vm
              , vmTranscript = vmTranscript vm
              , vmBeeps = vmBeeps vm
              , vmRng = vmRng vm
              }
  _ -> error "Grue.Interp.finishRestore: no restore is pending"

-- | Finish the innermost memory output stream: record the character
-- count in the table's first word, as @output_stream -3@ requires.
closeTable :: VM -> VM
closeTable vm = case vmTables vm of
  [] -> vm
  (table, count) : rest ->
    vm
      { vmMemory = pokeWord table (fromIntegral count) (vmMemory vm)
      , vmTables = rest
      }

-- | Start the story over, as the @restart@ opcode requires.  Flags 2
-- is preserved, as the standard demands (so a running transcript
-- keeps going); the random generator and unflushed output also
-- survive.
restart :: VM -> VM
restart vm =
  fresh
    { vmMemory = pokeWord 0x10 (peekWord (vmMemory vm) 0x10) (vmMemory fresh)
    , vmRng = vmRng vm
    , vmOutput = vmOutput vm
    , vmTranscript = vmTranscript vm
    , vmBeeps = vmBeeps vm
    }
  where
    fresh = boot (originalBytes (vmMemory vm))

-- | Complete a pending @read@: store the typed line in the text
-- buffer, tokenize it against the standard dictionary, and fill the
-- parse buffer.  The machine is left ready to 'run' again.  A running
-- transcript receives the input line, as the standard requires.
provideInput :: Text -> VM -> VM
provideInput input vm = case vmPending vm of
  Just (PendingRead tbuf pbuf) ->
    echoed {vmMemory = written, vmPending = Nothing}
    where
      echoed
        | transcriptOn vm = emitTranscript (input <> "\n") vm
        | otherwise = vm
      mem = vmMemory vm
      hdr = vmHeader vm
      maxLetters = fromIntegral (peekByte mem tbuf)
      line = T.map toLower (T.take maxLetters input)
      codes = mapMaybe charToZscii (T.unpack line)
      textWrites m =
        foldr
          (\(i, c) -> pokeByte (tbuf + 1 + i) (fromIntegral c))
          m
          (zip [0 ..] codes)
      terminated = pokeByte (tbuf + 1 + length codes) 0 . textWrites
      dict = readDictionary mem hdr
      tokens = take (fromIntegral (peekByte mem pbuf)) (tokenize dict line)
      entry i (pos, word) m =
        ( pokeWord base (fromIntegral dictAddr)
            . pokeByte (base + 2) (fromIntegral (T.length word))
            . pokeByte (base + 3) (fromIntegral (pos + 1))
        )
          m
        where
          base = pbuf + 2 + 4 * i
          dictAddr = fromMaybe 0 (lookupWord mem hdr dict word)
      parseWrites m =
        foldr (uncurry entry) m (zip [0 ..] tokens)
      counted = pokeByte (pbuf + 1) (fromIntegral (length tokens)) . parseWrites
      written = counted (terminated mem)
  _ -> error "Grue.Interp.provideInput: no read is pending"

-- | What the status line should currently show.
data StatusLine = StatusLine
  { statusRoom :: Text
    -- ^ The short name of the object in the first global variable,
    -- conventionally the current room.
  , statusRight :: RightStatus
  }
  deriving (Eq, Show)

-- | The right-hand side of the status line: score and turn count, or
-- the time of day for a "time game".
data RightStatus
  = ScoreMoves Int Int
  | HoursMins Int Int
  deriving (Eq, Show)

-- | Compute the current status line from the machine state.
statusLine :: VM -> StatusLine
statusLine vm = StatusLine room right
  where
    mem = vmMemory vm
    hdr = vmHeader vm
    global n = peekVar (16 + n) vm
    room = Obj.shortName mem hdr (fromIntegral (global 0))
    timeGame = testBit (peekByte mem 0x01) 1
    right
      | timeGame = HoursMins (fromIntegral (global 1)) (fromIntegral (global 2))
      | otherwise = ScoreMoves (signed (global 1)) (fromIntegral (global 2))
