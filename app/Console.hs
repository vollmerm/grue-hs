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

-- | Where the game transcript (output stream 2) goes.  The player is
-- asked for a file name the first time the story turns the transcript
-- on, and only once per session.
data ScriptFile = NotAsked | Declined | ScriptTo FilePath

-- | Run the story, flushing output and feeding input until it halts.
-- The transcript always ends with a newline, so a final prompt does
-- not run into the shell's.
play :: BS.ByteString -> IO ()
play story = do
  hSetBuffering stdout NoBuffering
  loop True NotAsked (boot story)
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
        NeedInput -> do
          withLine (finish atLineStart') $ \line ->
            loop atLineStart' script' (provideInput (T.strip line) vm'')
        SaveRequested bytes -> do
          putStr "Save to file: "
          withLine (loop True script' (finishSave False vm'')) $ \name -> do
            written <- try (BS.writeFile (T.unpack (T.strip name)) bytes)
            let ok = either (\e -> const False (e :: IOException)) (const True) written
            loop True script' (finishSave ok vm'')
        RestoreRequested -> do
          putStr "Restore from file: "
          withLine (loop True script' (finishRestore Nothing vm'')) $ \name -> do
            readBack <- try (BS.readFile (T.unpack (T.strip name)))
            let bytes = either (\e -> const Nothing (e :: IOException)) Just readBack
            loop True script' (finishRestore bytes vm'')
    finish atLineStart =
      if atLineStart then pure () else putStrLn ""
    withLine onEOF act = do
      eof <- hIsEOF stdin
      if eof then onEOF else act =<< TIO.getLine

-- | Write transcript text to its file, asking for the file name on
-- first use.  An empty name, end of input, or a write failure turns
-- the transcript file off for the rest of the session.
flushScript :: ScriptFile -> T.Text -> IO ScriptFile
flushScript script t
  | T.null t = pure script
  | otherwise = case script of
      Declined -> pure Declined
      ScriptTo path -> writeOrDecline path (TIO.appendFile path t)
      NotAsked -> do
        putStr "Script to file: "
        eof <- hIsEOF stdin
        if eof
          then pure Declined
          else do
            name <- TIO.getLine
            let path = T.unpack (T.strip name)
            if null path
              then pure Declined
              else writeOrDecline path (TIO.writeFile path t)
  where
    writeOrDecline path write = do
      written <- try write
      pure $ case written of
        Left e -> const Declined (e :: IOException)
        Right () -> ScriptTo path
