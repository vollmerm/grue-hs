{-# LANGUAGE OverloadedStrings #-}

-- | A full-screen curses interface.
--
-- The top row is the traditional version 3 status line (current room
-- on the left; score and turns, or the time, on the right).  The rest
-- of the screen scrolls story text, word-wrapped to the terminal
-- width, with line editing at the prompt.
module CursesUI (play) where

import Control.Exception (finally)
import Control.Monad (zipWithM_)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
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

play :: BS.ByteString -> IO ()
play story =
  (CursesHelper.start >> loop [""] (boot story))
    `finally` CursesHelper.end

loop :: Scrollback -> VM -> IO ()
loop buf vm0 = do
  let (out, stop, vm) = run vm0
      buf' = append buf out
  case stop of
    Halted -> do
      let final = append buf' "\n[The story has ended. Press any key.]"
      render vm final ""
      _ <- CursesHelper.getKey (render vm final "")
      pure ()
    NeedInput -> do
      line <- editLine vm buf'
      loop (append buf' (line <> "\n")) (provideInput line vm)

-- | Add output text to the scrollback, splitting at new-lines.
append :: Scrollback -> Text -> Scrollback
append [] t = append [""] t
append (open : rest) t = case T.split (== '\n') t of
  [] -> open : rest
  (s : ss) -> take scrollbackLimit (reverse ss ++ (open <> s) : rest)

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

-- | Redraw the whole screen: status line, story text, pending input.
render :: VM -> Scrollback -> Text -> IO ()
render vm buf input = do
  (rows, cols) <- Curses.scrSize
  Curses.erase
  drawStatus cols (statusLine vm)
  let width = max 20 (cols - 1)
      textRows = rows - 1
      wrapped = concatMap (wrapLine width) (reverse (withInput buf))
      visible = lastN textRows wrapped
  zipWithM_
    (\r line -> Curses.mvWAddStr Curses.stdScr r 0 (T.unpack line))
    [1 ..]
    visible
  let cursorRow = length visible
      cursorCol = maybe 0 T.length (lastMaybe visible)
  Curses.wMove Curses.stdScr (max 1 cursorRow) (min (cols - 1) cursorCol)
  Curses.refresh
  where
    withInput [] = [input]
    withInput (open : rest) = (open <> input) : rest
    lastN n xs = drop (max 0 (length xs - n)) xs
    lastMaybe xs = if null xs then Nothing else Just (last xs)

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
        printf "Time: %d:%02d " hours mins
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
