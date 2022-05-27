module Main where

import Connect4 (GameConfig (..), startGame, ParallelMode (..))
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["eval", "bench"] -> startGame GameConfig {parallelMode = Eval, benchMode = True, gameTreeDepth = 7}
    ["par", "bench"] -> startGame GameConfig {parallelMode = Par, benchMode = True, gameTreeDepth = 7}
    ["seq", "bench"] -> startGame GameConfig {parallelMode = Seq, benchMode = True, gameTreeDepth = 7}
    ["eval"] -> startGame GameConfig {parallelMode = Eval, benchMode = False, gameTreeDepth = 7}
    ["par"] -> startGame GameConfig {parallelMode = Par, benchMode = False, gameTreeDepth = 7}
    _ -> startGame GameConfig {parallelMode = Seq, benchMode = False, gameTreeDepth = 5}
  return ()
