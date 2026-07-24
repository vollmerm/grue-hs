-- | A plain standard-input\/output interface.
--
-- Output text is printed as it arrives and each read request takes one
-- line from stdin.  This suits piped, scripted use (for instance
-- comparing transcripts against another interpreter) as well as simple
-- interactive play.  No status line is shown.
module Console (play) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Files
import Grue.Interp
import Grue.VM
import System.IO (BufferMode (NoBuffering), hGetChar, hIsEOF, hSetBuffering, stdin, stdout)

-- | Run the story, flushing output and feeding input until it halts.
-- The transcript always ends with a newline, so a final prompt does
-- not run into the shell's.
play :: Word64 -> BS.ByteString -> IO ()
play seed story = do
  hSetBuffering stdout NoBuffering
  loop True NotAsked (bootWithSeed seed story)
  where
    loop atLineStart script vm = do
      let (out, stop, vm') = run vm
      TIO.putStr out
      let (scriptText, vm'') = takeTranscript vm'
      script' <- flushScript script scriptText
      let atLineStart' =
            if T.null out then atLineStart else T.last out == '\n'
      case stop of
        Halted -> finish atLineStart'
        NeedInput ->
          withLine (finish atLineStart') $ \line ->
            loop atLineStart' script' (provideInput (T.strip line) vm'')
        NeedChar -> do
          eof <- hIsEOF stdin
          if eof
            then finish atLineStart'
            else do
              c <- hGetChar stdin
              loop atLineStart' script' (provideChar (charZscii c) vm'')
        SaveRequested bytes -> do
          putStr "Save to file: "
          withLine (loop True script' (finishSave False vm'')) $ \name -> do
            ok <- writeSave name bytes
            loop True script' (finishSave ok vm'')
        RestoreRequested -> do
          putStr "Restore from file: "
          withLine (loop True script' (finishRestore Nothing vm'')) $ \name -> do
            bytes <- readSave name
            loop True script' (finishRestore bytes vm'')
    finish atLineStart =
      if atLineStart then pure () else putStrLn ""
    withLine onEOF act = do
      eof <- hIsEOF stdin
      if eof then onEOF else act =<< TIO.getLine
    -- read_char consumes a single byte, so scripted input stays in step
    -- with the reference interpreter.  A newline becomes ZSCII 13.
    charZscii c = if c == '\n' then 13 else fromIntegral (fromEnum c)

-- | Write transcript text to its file, asking for the file name on
-- first use.  An empty name, end of input, or a write failure turns
-- the transcript file off for the rest of the session.
flushScript :: ScriptFile -> T.Text -> IO ScriptFile
flushScript script t
  | T.null t = pure script
  | otherwise = case script of
      Declined -> pure Declined
      ScriptTo path -> appendScript path t
      NotAsked -> do
        putStr "Script to file: "
        eof <- hIsEOF stdin
        if eof
          then pure Declined
          else do
            name <- TIO.getLine
            maybe (pure Declined) (`startScript` t) (scriptPath name)
