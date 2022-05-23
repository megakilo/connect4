module Main where

import Connect4 (GameConfig (..), startGame)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["par", "bench"] -> startGame GameConfig {parallelMode = True, benchMode = True, gameTreeDepth = 7}
    ["seq", "bench"] -> startGame GameConfig {parallelMode = False, benchMode = True, gameTreeDepth = 7}
    ["par"] -> startGame GameConfig {parallelMode = True, benchMode = False, gameTreeDepth = 7}
    _ -> startGame GameConfig {parallelMode = False, benchMode = False, gameTreeDepth = 5}
  return ()
