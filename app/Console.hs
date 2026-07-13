-- | A plain standard-input\/output interface.
--
-- Output text is printed as it arrives and each read request takes one
-- line from stdin.  This suits piped, scripted use (for instance
-- comparing transcripts against another interpreter) as well as simple
-- interactive play.  No status line is shown.
module Console (play) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Grue.Interp
import Grue.VM
import System.IO (BufferMode (NoBuffering), hIsEOF, hSetBuffering, stdin, stdout)

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
          withLine (finish atLineStart') $ \line ->
            loop atLineStart' (provideInput (T.strip line) vm')
        SaveRequested bytes -> do
          putStr "Save to file: "
          withLine (loop True (finishSave False vm')) $ \name -> do
            written <- try (BS.writeFile (T.unpack (T.strip name)) bytes)
            let ok = either (\e -> const False (e :: IOException)) (const True) written
            loop True (finishSave ok vm')
        RestoreRequested -> do
          putStr "Restore from file: "
          withLine (loop True (finishRestore Nothing vm')) $ \name -> do
            readBack <- try (BS.readFile (T.unpack (T.strip name)))
            let bytes = either (\e -> const Nothing (e :: IOException)) Just readBack
            loop True (finishRestore bytes vm')
    finish atLineStart =
      if atLineStart then pure () else putStrLn ""
    withLine onEOF act = do
      eof <- hIsEOF stdin
      if eof then onEOF else act =<< TIO.getLine
