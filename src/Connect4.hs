{-# LANGUAGE RecordWildCards #-}

module Connect4 (startGame, GameConfig (..), ParallelMode (..)) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Par (Par, parMapM, runPar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, put)
import Control.Parallel.Strategies (Eval, parList, rdeepseq, rpar, using)
import Data.Char (isDigit)
import Data.List (isInfixOf, transpose)
import System.Process (system)

-- Grid state and current player
type GameState = (Grid, Player)

data ParallelMode = Seq | Par | Eval deriving (Eq)

data GameConfig = GameConfig
  { parallelMode :: ParallelMode,
    benchMode :: Bool,
    gameTreeDepth :: Int
  }

type GameM = StateT GameState (ReaderT GameConfig IO) ()

-- Game configs --
gridWidth = 7

gridHeight = 6

-- Types --
data Player = O | B | X
  deriving (Eq, Ord)

instance NFData Player where
  rnf x = x `seq` ()

instance Show Player where
  show O = " O "
  show B = "   "
  show X = " X "

type Column a = [a]

type Matrix a = [Column a]

type Grid = Matrix Player

-- Game --
startGame :: GameConfig -> IO ()
startGame config@GameConfig {..} = do
  let f = if benchMode then benchGame else playGame
  runReaderT (runStateT f (newGrid, O)) config
  return ()

benchGame :: GameM
benchGame = do
  config <- lift ask
  game@(grid, _) <- get
  liftIO $ putStrLn "Computer is thinking... "
  put $! bestmove config game
  (g, _) <- get
  liftIO $ putGrid g

playGame :: GameM
playGame = do
  config@GameConfig {..} <- lift ask
  game@(grid, _) <- get
  liftIO $ system "clear"
  liftIO $ putGrid grid
  makeMove config game
  where
    makeMove config game@(grid, player)
      | wins O grid = liftIO $ putStrLn "You win!"
      | wins X grid = liftIO $ putStrLn "Computer wins!"
      | player == O = do
        i <- liftIO $ getUserInput grid
        put $ addMove game i
        playGame
      | otherwise = do
        liftIO $ putStrLn "Computer is thinking... "
        put $! bestmove config game
        playGame

getUserInput :: Grid -> IO Int
getUserInput grid = do
  putStr "Enter your move: "
  xs <- getLine
  case getColumn xs of
    Just i -> return i
    Nothing -> do
      putStrLn "ERROR: Invalid move"
      getUserInput grid
  where
    getColumn str = do
      guard $ str /= [] && all isDigit str
      let i = read str - 1
      if valid grid i
        then return i
        else Nothing

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid --
newGrid :: Grid
newGrid = replicate gridWidth (replicate gridHeight B)

full :: Grid -> Bool
full = notElem B . fmap head

wins :: Player -> Grid -> Bool
wins p g = any hasFourInRow (g ++ transpose g ++ dias)
  where
    hasFourInRow col = replicate 4 p `isInfixOf` col
    dias = diag g ++ diag (map reverse g)

won :: Grid -> Bool
won g = wins O g || wins X g

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < gridWidth && head (g !! i) == B

addMove :: GameState -> Int -> GameState
addMove (grid, player) i = (newGrid, next player)
  where
    (xs, orig : ys) = splitAt i grid
    current = dropWhile (== B) orig
    updated = replicate (gridHeight - length current - 1) B ++ player : current
    newGrid = xs ++ [updated] ++ ys

diag :: Show a => Matrix a -> [Column a]
diag g = do
  let size = min gridHeight gridWidth
  c <- [0 .. gridWidth -4]
  r <- [0 .. gridHeight -4]
  return [g !! (c + k) !! (r + k) | k <- [0 .. size -1], c + k < gridWidth, r + k < gridHeight]

-- UI --
putGrid :: Show a => Matrix a -> IO ()
putGrid m = do
  putStr . unlines . interleave divider . map showRow . transpose $ m
  putStrLn $ showIndex ++ "\n"
  where
    divider = replicate ((gridWidth * 4) + 1) '-'
    showIndex = concat $ interleave " " [" " ++ show i ++ " " | i <- [1 .. gridWidth]]
    showRow = concat . interleave "|" . map show

interleave :: a -> [a] -> [a]
interleave x [] = [x]
interleave x (y : ys) = x : y : interleave x ys

-- Minmax --
data Tree a = Node a [Tree a]

instance NFData a => NFData (Tree a) where
  rnf (Node x ts) = rnf x `seq` rnf ts

moves :: GameState -> [GameState]
moves game@(grid, _)
  | won grid = []
  | full grid = []
  | otherwise = [addMove game i | i <- [0 .. gridWidth -1], valid grid i]

gametree :: GameState -> Tree GameState
gametree game = Node game [gametree g' | g' <- moves game]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n -1) t | t <- ts]

minimax :: ParallelMode -> Tree GameState -> Tree (GameState, Player)
minimax _ (Node game@(grid, player) [])
  | wins O grid = Node (game, O) []
  | wins X grid = Node (game, X) []
  | otherwise = Node (game, B) []
minimax par (Node game@(grid, player) ts) = Node (game, cmp ps) ts'
  where
    cmp = if player == O then minimum else maximum
    ts' =
      if par == Eval
        then map (minimax par) ts `using` parList rdeepseq
        else map (minimax par) ts
    ps = [p | Node (_, p) _ <- ts']

minimaxPar :: Tree GameState -> Par (Tree (GameState, Player))
minimaxPar (Node game@(grid, player) [])
  | wins O grid = return $ Node (game, O) []
  | wins X grid = return $ Node (game, X) []
  | otherwise = return $ Node (game, B) []
minimaxPar (Node game@(grid, player) ts) = do
  ts' <- parMapM minimaxPar ts
  let ps = [p | Node (_, p) _ <- ts']
  let cmp = if player == O then minimum else maximum
  return $ Node (game, cmp ps) ts'

bestmove :: GameConfig -> GameState -> GameState
bestmove GameConfig {..} gs = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune gameTreeDepth (gametree gs)
    Node (_, best) ts =
      if parallelMode == Par
        then runPar $ minimaxPar tree
        else minimax parallelMode tree
