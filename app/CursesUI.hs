{-# LANGUAGE OverloadedStrings #-}

-- | A full-screen curses interface.
--
-- The top row is the traditional version 3 status line (current room
-- on the left; score and turns, or the time, on the right).  The rest
-- of the screen scrolls story text, word-wrapped to the terminal
-- width, with line editing at the prompt.
module CursesUI (play) where

import Control.Exception (IOException, finally, try)
import Control.Monad (replicateM_, zipWithM_)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Grue.Interp
import Grue.VM
import Text.Printf (printf)
import UI.HSCurses.Curses qualified as Curses
import UI.HSCurses.CursesHelper qualified as CursesHelper

-- | Story text shown so far, as logical (unwrapped) lines with the
-- newest first.  The head is the line still being added to.
type Scrollback = [Text]

-- | How much scrollback to retain.
scrollbackLimit :: Int
scrollbackLimit = 500

-- | Where the game transcript (output stream 2) goes.  The player is
-- asked for a file name the first time the story turns the transcript
-- on, and only once per session.
data ScriptFile = NotAsked | Declined | ScriptTo FilePath

play :: BS.ByteString -> IO ()
play story =
  (CursesHelper.start >> loop NotAsked [""] (boot story))
    `finally` CursesHelper.end

loop :: ScriptFile -> Scrollback -> VM -> IO ()
loop script buf vm0 = do
  let (out, stop, vm1) = run vm0
      (scriptText, vm2) = takeTranscript vm1
      (beeps, vm) = takeBeeps vm2
  replicateM_ beeps Curses.beep
  paged <- addOutput vm buf out
  (script', buf') <- flushScript vm script paged scriptText
  case stop of
    Halted -> do
      let final = append buf' "\n[The story has ended. Press any key.]"
      render vm final ""
      _ <- CursesHelper.getKey (render vm final "")
      pure ()
    NeedInput -> do
      line <- editLine vm buf'
      loop script' (append buf' (line <> "\n")) (provideInput line vm)
    SaveRequested bytes -> do
      let prompt = append buf' "Save to file: "
      name <- editLine vm prompt
      let buf'' = append prompt (name <> "\n")
      written <- try (BS.writeFile (T.unpack (T.strip name)) bytes)
      let ok = either (\e -> const False (e :: IOException)) (const True) written
      loop script' buf'' (finishSave ok vm)
    RestoreRequested -> do
      let prompt = append buf' "Restore from file: "
      name <- editLine vm prompt
      let buf'' = append prompt (name <> "\n")
      readBack <- try (BS.readFile (T.unpack (T.strip name)))
      let bytes = either (\e -> const Nothing (e :: IOException)) Just readBack
      loop script' buf'' (finishRestore bytes vm)

-- | Write transcript text to its file, asking for the file name on
-- first use.  An empty name or a write failure turns the transcript
-- file off for the rest of the session.
flushScript :: VM -> ScriptFile -> Scrollback -> Text -> IO (ScriptFile, Scrollback)
flushScript vm script buf t
  | T.null t = pure (script, buf)
  | otherwise = case script of
      Declined -> pure (Declined, buf)
      ScriptTo path -> do
        script' <- writeOrDecline path (TIO.appendFile path t)
        pure (script', buf)
      NotAsked -> do
        let prompt = append buf "Script to file: "
        name <- editLine vm prompt
        let buf' = append prompt (name <> "\n")
            path = T.unpack (T.strip name)
        if null path
          then pure (Declined, buf')
          else do
            script' <- writeOrDecline path (TIO.writeFile path t)
            pure (script', buf')
  where
    writeOrDecline path write = do
      written <- try write
      pure $ case written of
        Left e -> const Declined (e :: IOException)
        Right () -> ScriptTo path

-- | Add output text to the scrollback, splitting at new-lines.
append :: Scrollback -> Text -> Scrollback
append [] t = append [""] t
append (open : rest) t = case T.split (== '\n') t of
  [] -> open : rest
  (s : ss) -> take scrollbackLimit (reverse ss ++ (open <> s) : rest)

-- | Add story output to the scrollback, pausing on a "[MORE]" prompt
-- whenever more than a screenful arrives at once, so nothing scrolls
-- past unread.
addOutput :: VM -> Scrollback -> Text -> IO Scrollback
addOutput vm buf out = do
  (rows, cols) <- Curses.scrSize
  let width = max 20 (cols - 1)
      top = 1 + upperHeight (vmUpper vm)
      textRows = max 1 (rows - top)
      pageRows = max 1 (textRows - 1)
      buf' = append buf out
      allRows = concatMap (wrapLine width) (reverse buf')
      total = length allRows
      -- How many display rows still need reading: the rows of every
      -- line the output touches, including the open line it extends.
      open = case buf of
        (o : _) -> o
        [] -> ""
      touched = case T.split (== '\n') out of
        [] -> []
        (s : ss) -> (open <> s) : ss
      new = sum (map (length . wrapLine width) touched)
      reveal k = do
        Curses.erase
        _ <- drawTop vm cols
        zipWithM_
          (\r line -> Curses.mvWAddStr Curses.stdScr r 0 (T.unpack line))
          [top ..]
          (lastN pageRows (take k allRows))
        Curses.wAttrSet Curses.stdScr (Curses.setReverse Curses.attr0 True, Curses.Pair 0)
        Curses.mvWAddStr Curses.stdScr (rows - 1) 0 "[MORE]"
        Curses.wAttrSet Curses.stdScr (Curses.attr0, Curses.Pair 0)
        Curses.refresh
      page k
        | total - k <= textRows = pure buf'
        | otherwise = do
            let k' = min total (k + pageRows)
            reveal k'
            _ <- CursesHelper.getKey (reveal k')
            page k'
  page (max 0 (total - new))

-- | Read one line of input, echoing at the end of the scrollback.
editLine :: VM -> Scrollback -> IO Text
editLine vm buf = edit ""
  where
    edit input = do
      render vm buf input
      key <- CursesHelper.getKey (render vm buf input)
      case key of
        Curses.KeyChar '\n' -> pure input
        Curses.KeyChar '\r' -> pure input
        Curses.KeyEnter -> pure input
        Curses.KeyBackspace -> edit (T.dropEnd 1 input)
        Curses.KeyChar c
          | c == '\b' || c == '\DEL' -> edit (T.dropEnd 1 input)
          | c >= ' ' -> edit (input <> T.singleton c)
        _ -> edit input

-- | Redraw the whole screen: status line, any upper window, story
-- text, pending input.
render :: VM -> Scrollback -> Text -> IO ()
render vm buf input = do
  (rows, cols) <- Curses.scrSize
  Curses.erase
  top <- drawTop vm cols
  let width = max 20 (cols - 1)
      textRows = max 1 (rows - top)
      wrapped = concatMap (wrapLine width) (reverse (withInput buf))
      visible = lastN textRows wrapped
  zipWithM_
    (\r line -> Curses.mvWAddStr Curses.stdScr r 0 (T.unpack line))
    [top ..]
    visible
  let cursorRow = top + length visible - 1
      cursorCol = maybe 0 T.length (lastMaybe visible)
  Curses.wMove Curses.stdScr (max top cursorRow) (min (cols - 1) cursorCol)
  Curses.refresh
  where
    withInput [] = [input]
    withInput (open : rest) = (open <> input) : rest
    lastMaybe xs = if null xs then Nothing else Just (last xs)

-- | Draw the fixed top of the screen -- the status line and the
-- story's upper window, if split -- returning the first row left for
-- scrolling story text.
drawTop :: VM -> Int -> IO Int
drawTop vm cols = do
  drawStatus cols (statusLine vm)
  zipWithM_
    (\r line ->
       Curses.mvWAddStr Curses.stdScr r 0 (T.unpack (T.take (cols - 1) line)))
    [1 ..]
    (toList (upperLines (vmUpper vm)))
  pure (1 + upperHeight (vmUpper vm))

-- | The last @n@ elements of a list.
lastN :: Int -> [a] -> [a]
lastN n xs = drop (max 0 (length xs - n)) xs

-- | Draw the reverse-video status bar on the top row.
drawStatus :: Int -> StatusLine -> IO ()
drawStatus cols status = do
  Curses.wAttrSet Curses.stdScr (Curses.setReverse Curses.attr0 True, Curses.Pair 0)
  Curses.mvWAddStr Curses.stdScr 0 0 (T.unpack line)
  Curses.wAttrSet Curses.stdScr (Curses.attr0, Curses.Pair 0)
  where
    left = " " <> statusRoom status
    right = T.pack $ case statusRight status of
      ScoreMoves score moves ->
        printf "Score: %d  Moves: %d " score moves
      HoursMins hours mins ->
        let (hour, half)
              | hours == 0 = (12 :: Int, "am")
              | hours < 12 = (hours, "am")
              | hours == 12 = (12, "pm")
              | otherwise = (hours - 12, "pm")
         in printf "Time: %d:%02d %s " hour mins (half :: String)
    gap = cols - T.length left - T.length right
    line
      | gap >= 1 = left <> T.replicate gap " " <> right
      | otherwise = T.take (cols - 1) (left <> " " <> right)

-- | Wrap one logical line to a width, breaking at spaces where
-- possible.  An empty line still occupies one screen row.
wrapLine :: Int -> Text -> [Text]
wrapLine width t
  | T.length t <= width = [t]
  | otherwise = line : wrapLine width rest
  where
    (chunk, overflow) = T.splitAt width t
    (line, rest) = case T.breakOnEnd " " chunk of
      (kept, tail')
        | T.null kept -> (chunk, overflow)
        | otherwise -> (T.stripEnd kept, tail' <> overflow)
