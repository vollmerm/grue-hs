-- | Command-line entry point for the grue-hs Z-machine interpreter.
--
-- Plays a story file with the full-screen curses interface when
-- attached to a terminal, or a plain stdin\/stdout loop when piped
-- (or when @--console@ is given).
module Main (main) where

import Console qualified
import Control.Monad (when)
import CursesUI qualified
import Data.ByteString qualified as BS
import Data.Word (Word64)
import Numeric (showHex)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin, stdout)
import System.Random (randomIO)

-- | Parsed command line: whether to force the console interface, an
-- optional fixed random seed, and the story path.
data Options = Options
  { optConsole :: Bool
  , optSeed :: Maybe Word64
  , optPath :: FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  case parse args of
    Just opts -> do
      story <- BS.readFile (optPath opts)
      seed <- resolveSeed (optSeed opts)
      interactive <-
        (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
      if interactive && not (optConsole opts)
        then CursesUI.play seed story
        else Console.play seed story
    Nothing -> do
      name <- getProgName
      putStrLn ("usage: " ++ name ++ " [--console] [--seed N] STORY-FILE")
      exitFailure

-- | Use the seed the player pinned, or draw a fresh one for genuine
-- unpredictability.  A drawn seed is announced on the error stream (so
-- it stays out of the transcript) when that stream is a terminal, so a
-- session can be reproduced later with @--seed@.
resolveSeed :: Maybe Word64 -> IO Word64
resolveSeed (Just s) = pure s
resolveSeed Nothing = do
  s <- randomIO
  tty <- hIsTerminalDevice stderr
  when tty $
    hPutStrLn stderr ("grue: random seed 0x" ++ showHex s " (pass --seed to reproduce)")
  pure s

-- | Accept @--console@ and @--seed N@ in any position around the story
-- path.  A seed may be given in decimal or with an @0x@ prefix.
parse :: [String] -> Maybe Options
parse = go False Nothing Nothing
  where
    go console seed (Just path) [] = Just (Options console seed path)
    go _ _ Nothing [] = Nothing
    go console seed path (arg : rest) = case arg of
      "--console" -> go True seed path rest
      "--seed" -> case rest of
        (n : more) | Just s <- readSeed n -> go console (Just s) path more
        _ -> Nothing
      _
        | Nothing <- path -> go console seed (Just arg) rest
        | otherwise -> Nothing
    readSeed s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing
