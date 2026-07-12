-- | Command-line entry point for the grue-hs Z-machine interpreter.
--
-- Plays a story file with the full-screen curses interface when
-- attached to a terminal, or a plain stdin\/stdout loop when piped
-- (or when @--console@ is given).
module Main (main) where

import Console qualified
import CursesUI qualified
import Data.ByteString qualified as BS
import Data.List (partition)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, stdin, stdout)

main :: IO ()
main = do
  args <- getArgs
  case parse args of
    Just (console, path) -> do
      story <- BS.readFile path
      interactive <-
        (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
      if interactive && not console
        then CursesUI.play story
        else Console.play story
    Nothing -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " [--console] STORY-FILE")
      exitFailure

-- | Accept @--console@ in any position around the story path.
parse :: [String] -> Maybe (Bool, FilePath)
parse args = case partition (== "--console") args of
  (flags, [path]) -> Just (not (null flags), path)
  _ -> Nothing
