-- | File access shared by the frontends.
--
-- Save files and the transcript script file are ordinary files named
-- by the player.  A failure to read or write is an outcome to report
-- to the story, never an exception, so every operation here catches
-- 'IOException' and answers with an ordinary value.
module Files
  ( -- * Save files
    writeSave
  , readSave

    -- * The script file (output stream 2)
  , ScriptFile (..)
  , scriptPath
  , startScript
  , appendScript
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Run an action, reporting failure as 'Nothing'.
attempt :: IO a -> IO (Maybe a)
attempt act = either discard Just <$> try act
  where
    discard :: IOException -> Maybe a
    discard _ = Nothing

-- | Write save bytes to the file the player named, reporting whether
-- the write succeeded.
writeSave :: Text -> ByteString -> IO Bool
writeSave name bytes = isJust <$> attempt (BS.writeFile (T.unpack (T.strip name)) bytes)

-- | Read back the save file the player named, if it can be read.
readSave :: Text -> IO (Maybe ByteString)
readSave name = attempt (BS.readFile (T.unpack (T.strip name)))

-- | Where the game transcript (output stream 2) goes.  The player is
-- asked for a file name the first time the story turns the transcript
-- on, and only once per session.
data ScriptFile = NotAsked | Declined | ScriptTo FilePath

-- | The file path in a player's answer to the script prompt, if the
-- answer named one.
scriptPath :: Text -> Maybe FilePath
scriptPath name
  | T.null stripped = Nothing
  | otherwise = Just (T.unpack stripped)
  where
    stripped = T.strip name

-- | Begin the script file with its first chunk of text, declining for
-- the rest of the session if the write fails.
startScript :: FilePath -> Text -> IO ScriptFile
startScript path t = outcome path <$> attempt (TIO.writeFile path t)

-- | Add text to the already-started script file, declining for the
-- rest of the session if the write fails.
appendScript :: FilePath -> Text -> IO ScriptFile
appendScript path t = outcome path <$> attempt (TIO.appendFile path t)

outcome :: FilePath -> Maybe () -> ScriptFile
outcome path = maybe Declined (const (ScriptTo path))
