module Connect4 (startGame) where

import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Foldable (find)
import Data.Function (on)
import Data.List (isInfixOf, sortBy, transpose)
import Data.Maybe (fromMaybe)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Process (system)

startGame :: IO ()
startGame = do
  hSetBuffering stdout NoBuffering
  game newGrid O

-- Game configs --
gridWidth = 7

gridHeight = 6

gameTreeDepth = 5

-- Types --
data Player = O | B | X
  deriving (Eq, Ord)

instance Show Player where
  show O = " O "
  show B = "   "
  show X = " X "

type Column a = [a]

type Matrix a = [Column a]

type Grid = Matrix Player

-- Game --
next :: Player -> Player
next O = X
next B = B
next X = O

game :: Grid -> Player -> IO ()
game g p = do
  system "clear"
  putGrid g
  play g p

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return $ read xs - 1
    else do
      putStrLn "ERROR: Invalid move"
      getNat prompt

play :: Grid -> Player -> IO ()
play g p
  | wins O g = putStrLn "You win!\n"
  | wins X g = putStrLn "Computer wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat "Enter your move: "
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play g p
      [g1] -> game g1 (next p)
  | p == X = do
    putStr "Computer is thinking... "
    (game $! bestmove g p) (next p)

-- Grid --
newGrid :: Grid
newGrid = replicate gridWidth (replicate gridHeight B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any hasFourInRow (g ++ transpose g ++ dias)
  where
    hasFourInRow col = replicate 4 p `isInfixOf` col
    dias = diag g ++ diag (map reverse g)

won :: Grid -> Bool
won g = wins O g || wins X g

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < gridWidth && head (g !! i) == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [xs ++ [updated] ++ ys] else []
  where
    (xs, orig : ys) = splitAt i g
    updated = addMove orig p

addMove :: Column Player -> Player -> Column Player
addMove col p = replicate (gridHeight - length current - 1) B ++ [p] ++ current
  where
    current = dropWhile (== B) col

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

showRow :: Show a => [a] -> String
showRow = concat . interleave "|" . map show

showIndex :: String
showIndex = concat $ interleave " " [" " ++ show i ++ " " | i <- [1 .. gridWidth]]

interleave :: a -> [a] -> [a]
interleave x [] = [x]
interleave x (y : ys) = x : y : interleave x ys

-- Minmax --
data Tree a = Node a [Tree a]
  deriving (Show)

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. gridWidth -1], valid g i]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n -1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune gameTreeDepth (gametree g p)
    Node (_, best) ts = minimax tree