{-
1.  3*3 の3目並べを空の格子から始めると，完全なゲームの木の節の数が
    549,946 になることを，関数 gametree を使って確かめましょう．
    また，木の最大の深さが 9 であることを確認してください．
-}

import Data.Char
import Data.List
import System.IO


--- 解答
-- gametree を受け取り，その大きさを返す関数を作れば良い
tsize :: Tree a -> Int
tsize (Node _ []) = 1
tsize (Node _ xs) = 1 + sum (map tsize xs)

sol = tsize (gametree empty O)


--- おまけ

-- Grid を表示したときにどのマスがどの番号なのか一目でわかるように Player の定義を少し変更する
data Player = O | B Int | X
instance Eq Player where
  O == O = True
  (B _) == (B _) = True
  X == X = True
  _ == _ = False
instance Ord Player where
  O < B _ = True
  B _ < X = True
  O < X = True
  _ < _ = False

  p <= q = p < q || p == q
  p > q = q < p
  p >= q = q <= p
instance Show Player where
  show O = "o"
  show (B i) = show i
  show X = "x"

type Grid = [[Player]]

size :: Int
size = 3

empty :: Grid
empty = chop size [B i | i <- [0..size^2-1]]

next :: Player -> Player
next O = X
next X = O
next (B i) = B i

written :: Player -> Bool
written O = True
written X = True 
written (B _) = False

finished :: Grid -> Bool
finished = all written . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any (all (== p)) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g


putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar  . map showRow
  where bar = [replicate (size*4-1) '-']


showRow :: [Player] -> [String]
showRow = join . interleave bar . map showPlayer
  where
    bar = replicate 3 "|"
    join = foldr1 (zipWith (++))
    

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer (B i) = ["   ", " " ++ show i ++ " ", "   "]
showPlayer X = ["   ", " X ", "   "]

data Tree a = Node a [Tree a]
              deriving Show

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B i

move :: Grid -> Player -> Int -> [Grid]
move g p i = if valid g i then [g'] else []
  where
    g' = chop size (l ++ [p] ++ r)
    (l, (B _):r) = splitAt i (concat g)


getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs then
    return (read xs)
  else
    do  putStrLn "ERROR: Invalid number"
        getNat prompt

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | finished g = []
  | otherwise = concat [move g p i | i <- [0..size^2-1]]

depth :: Int
depth = 2

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,B (-1)) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
  where
    t = prune depth (gametree g p)
    Node (_, best) ts = minimax t

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
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | finished g = putStrLn "It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g p i of
        [] -> do  putStrLn "ERROR: invalid move"
                  play' g p
        [g'] -> play g' (next p)
  | p == X = do
      putStrLn "Player X is thinking..."
      (play $! (bestmove g p)) (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


---------- auxiliary function ---------------
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y:x: interleave x ys

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


------ test ------
join :: [[String]] -> [String]
join = foldr1 (zipWith (++))

a = ["A", "B", "C"]
b = ["a", "b"]
c = ["1", "2", "3", "4"]

    