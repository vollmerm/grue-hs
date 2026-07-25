-- | A plain standard-input\/output interface.
--
-- Output text is printed as it arrives and each read request takes one
-- line from stdin.  This suits piped, scripted use (for instance
-- comparing transcripts against another interpreter) as well as simple
-- interactive play.  No status line is shown.
module Console (play) where

import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Files
import Grue.Interp
import Grue.Memory (peekByte)
import Grue.VM
import Grue.ZString (charToZsciiInStory)
import System.IO (BufferMode (NoBuffering), hGetChar, hIsEOF, hIsTerminalDevice, hSetBuffering, stdin, stdout)

-- | Run the story, flushing output and feeding input until it halts.
-- The transcript always ends with a newline, so a final prompt does
-- not run into the shell's.
play :: Word64 -> BS.ByteString -> IO ()
play seed story = do
  hSetBuffering stdout NoBuffering
  loop 0 NotAsked emptyUpper (bootWithSeed seed story)
  where
    loop col script prevUpper vm = do
      let (out, stop, vm') = run vm
          nextUpper = upperSnapshot vm'
          prefix = renderUpperPrefix col prevUpper nextUpper out vm'
      TIO.putStr prefix
      TIO.putStr out
      let (scriptText, vm'') = takeTranscript vm'
      script' <- flushScript script scriptText
      let col' = advanceColumn (advanceColumn col prefix) out
      case stop of
        Halted -> finish col'
        NeedInput ->
          let suffix = renderUpperPrompt col' nextUpper vm''
              col'' = advanceColumn col' suffix
           in do
                TIO.putStr suffix
                withInputLine vm'' (finish col'') $ \(line, term) -> do
                  newline <- needsInputNewline term
                  if newline then putStrLn "" else pure ()
                  loop (if newline then 0 else col'') script' nextUpper (provideInputTerminated term line vm'')
        NeedChar -> do
          eof <- hIsEOF stdin
          if eof
            then finish col'
            else do
              c <- hGetChar stdin
              loop col' script' nextUpper (provideChar (charZscii c) vm'')
        SaveRequested bytes -> do
          putStr "Save to file: "
          withLine (loop 0 script' nextUpper (finishSave False vm'')) $ \name -> do
            ok <- writeSave name bytes
            loop 0 script' nextUpper (finishSave ok vm'')
        RestoreRequested -> do
          putStr "Restore from file: "
          withLine (loop 0 script' nextUpper (finishRestore Nothing vm'')) $ \name -> do
            bytes <- readSave name
            loop 0 script' nextUpper (finishRestore bytes vm'')
    finish col' =
      if col' == 0 then pure () else putStrLn ""
    withLine onEOF act = do
      eof <- hIsEOF stdin
      if eof then onEOF else act =<< TIO.getLine
    withInputLine vm onEOF act = do
      eof <- hIsEOF stdin
      if eof then onEOF else act =<< readInputLine vm
    readInputLine vm = go T.empty
      where
        hdr = vmHeader vm
        mem = vmMemory vm
        terminators = inputTerminators vm
        go acc = do
          eof <- hIsEOF stdin
          if eof
            then pure (acc, 13)
            else do
              c <- hGetChar stdin
              case charToZsciiInStory mem hdr c of
                Just code
                  | code `elem` terminators -> pure (acc, code)
                _ -> go (acc <> T.singleton c)
    needsInputNewline term
      | term == 13 = pure False
      | otherwise = hIsTerminalDevice stdin
    -- read_char consumes a single byte, so scripted input stays in step
    -- with the reference interpreter.  A newline becomes ZSCII 13.
    charZscii c = if c == '\n' then 13 else fromIntegral (fromEnum c)

emptyUpper :: [T.Text]
emptyUpper = []

upperSnapshot :: VM -> [T.Text]
upperSnapshot =
  filter (not . T.null . T.strip)
    . map T.stripEnd
    . toList
    . upperLines
    . vmUpper

renderUpperPrefix :: Int -> [T.Text] -> [T.Text] -> T.Text -> VM -> T.Text
renderUpperPrefix col prev next out vm
  | next == prev = T.empty
  | col /= 0 = renderInlineUpper col next vm
  | null fresh = T.empty
  | otherwise = T.unlines fresh
  where
    fresh = filter (not . (`T.isInfixOf` out)) next

renderUpperPrompt :: Int -> [T.Text] -> VM -> T.Text
renderUpperPrompt col rows vm
  | null rows = T.empty
  | otherwise = renderInlineUpper col rows vm

renderInlineUpper :: Int -> [T.Text] -> VM -> T.Text
renderInlineUpper _ [] _ = T.empty
renderInlineUpper col rows vm =
  padToBoundary col width
    <> go rows
  where
    width = consoleWidth vm
    go :: [T.Text] -> T.Text
    go [] = T.empty
    go [row] = row
    go (row : rest) =
      row
        <> padToBoundary (T.length row) width
        <> go rest

consoleWidth :: VM -> Int
consoleWidth vm =
  max 1 $
    fromIntegral $
      peekByte (vmMemory vm) 0x21

padToBoundary :: Int -> Int -> T.Text
padToBoundary col width
  | rem' == 0 = T.empty
  | otherwise = T.replicate (width - rem') (T.singleton ' ')
  where
    rem' = col `mod` width

advanceColumn :: Int -> T.Text -> Int
advanceColumn col t = foldl step col (T.unpack t)
  where
    step _ '\n' = 0
    step n _ = n + 1

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
