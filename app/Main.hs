-- | Command-line entry point for the grue-hs Z-machine interpreter.
--
-- Runs a story file on standard input and output: output text is
-- printed as it arrives and each read request takes one line from
-- stdin.  This plain interface suits piped, scripted use (for instance
-- comparing transcripts against another interpreter) as well as
-- simple interactive play.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Grue.Interp
import Grue.VM
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hIsEOF, hSetBuffering, stdin, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> play =<< BS.readFile path
    _ -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " STORY-FILE")
      exitFailure

-- | Run the story, flushing output and feeding input until it halts.
-- The transcript always ends with a newline, so a final prompt does
-- not run into the shell's.
play :: BS.ByteString -> IO ()
play story = do
  hSetBuffering stdout NoBuffering
  loop True (boot story)
  where
    loop atLineStart vm = do
      let (out, stop, vm') = run vm
      TIO.putStr out
      let atLineStart' =
            if T.null out then atLineStart else T.last out == '\n'
      case stop of
        Halted -> finish atLineStart'
        NeedInput -> do
          eof <- hIsEOF stdin
          if eof
            then finish atLineStart'
            else do
              line <- TIO.getLine
              loop atLineStart' (provideInput (T.strip line) vm')
    finish atLineStart = do
      if atLineStart then pure () else putStrLn ""
      exitSuccess
