{-
2.  最善手が複数あるときに，それらからランダムに手が選ばれるようにしてください．
    System.Random モジュールが提供する関数 randomRIO :: (Int, Int) -> IO Int
    を用いてください．
-}

import System.Random
import Data.Char
import Data.List
import System.IO

---------- test begin ----------
test1 :: IO ()
test1 = do
  x <- (randomRIO (0, 10) :: IO Int)
  putStr (show x)
---------- test end ------------

data Player = O | B Int | X
instance Eq Player where
  O == O = True
  B _ == B _ = True
  X == X = True 
  _ == _ = False
instance Ord Player where
  O < B _ = True
  B _ < X = True 
  O < X = True
  _ < _ = False
  b <= c = b < c || b == c
  b > c = c < b
  b >= c = c <= b
instance Show Player where
  show O = "o"  
  show (B i) = show i
  show X = "x"

type Grid = [[Player]]
data Tree a = Node a [Tree a]
              deriving Show

size :: Int
size = 3

depth :: Int
depth = 9

empty :: Grid
empty = chop size [B i | i <- [0..size^2-1]]

nextp :: Player -> Player
nextp O = X
nextp X = O
nextp (B i) = B i

full :: Grid -> Bool
full = all written . concat
  where
    written p = p == O || p == X

wins :: Grid -> Player -> Bool
wins g p = any complete lines
  where
    complete = all (== p)
    lines = rows ++ cols ++ dias
    rows = g
    cols = transpose g
    dias = [dia g, dia (map reverse g)]
    dia g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins g O || wins g X

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate (size*4-1) '-']

showRow :: [Player] -> [String]
showRow = assemble . interleave bar . map showPlayer
  where
    bar = replicate 3 "|"
    assemble = foldr1 (zipWith (++))

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer (B i) = [show i ++ "  ", "   ", "   "]


valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B i

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (ls ++ [p] ++ rs)] else []
  where (ls, _:rs) = splitAt i (concat g)

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..size^2-1]]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (nextp p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node g _) = Node g []
prune n (Node g ts) = Node g (map (prune (n-1)) ts)


type Metric = (Player,Int)

minimax :: Tree Grid -> Tree (Grid,Metric)
minimax (Node g [])
  | wins g O = Node (g,(O,0)) []
  | wins g X = Node (g,(X,0)) []
  | otherwise = Node (g,(B (-1),0)) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
      where
        ts' = [minimax t | t <- ts]
        ps = [p | Node (_,p) _ <- ts']

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = opts
  where
    gt = prune depth (gametree g p)
    Node (_,optimal) ts = minimax gt
    opts = [g' | Node(g',opt) _ <- ts, opt == optimal]


getNum :: String -> IO Int
getNum prompt = do
  putStr prompt
  x <- getLine
  if x /= [] && all isDigit x then
    return (read x)
  else
    do  putStrLn "ERROR: invalid number"
        getNum prompt


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O


play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1,1)
  putGrid g
  play' g p


play' :: Grid -> Player -> IO ()
play' g p
  | wins g O = putStrLn "Player O wins!!\n" 
  | wins g X = putStrLn "Player X wins!!\n"
  | full g = putStrLn "It's a draw...\n"
  | p == O = do
      i <- getNum (prompt p)
      case move g i p of
        [] -> do  putStrLn "ERROR: invalid move"
                  play' g p
        [g'] -> play g' (nextp p)
  | p == X = do
      putStr "Player X is thinking..."
      let gs = bestmoves g p
      let range = length gs
      i <- randomRIO (0,range-1)
      play (gs !! i) (nextp p)


prompt :: Player -> String
prompt p = "Player " ++ show p ++ " Enter your move: "







----------- helper function ---------------
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:ys) = y:x : interleave x ys

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


-- 最短で勝ってくる一例
test3 :: IO ()
test3 = do
  hSetBuffering stdout NoBuffering
  play g1 X

g1 = [[B 0, O, X],
      [O, X, B 5],
      [B 6, O, X]]