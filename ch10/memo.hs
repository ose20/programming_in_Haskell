import System.IO
import Data.Char
import Distribution.Simple.Build (initialBuildSteps)

hangman :: IO ()
hangman = do  putStrLn' "think of a word:"
              word <- sgetLine
              putStrLn' "Try to guess it."
              play word

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' s = do  putStr' s
                  putChar '\n'

match :: String -> String -> String
match [] _ = []
match (x:xs) [] = '-' : match xs []
match (x:xs) (y:ys) | x==y = x : match xs ys
                    | otherwise  = '-' : match xs ys

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do  putChar x
                    return []
              else
                do  putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x


play :: String -> IO ()
play word = do  putStr' "? "
                chlng <- sgetLine
                if word == chlng then
                  putStrLn' "You got it"
                else
                  do  putStrLn' (match word chlng)
                      play word


-- ニム
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move b row num = [update r n| (r, n) <- zip [1..] b]
  where update r n = if r == row then n-num else n


putBoardline :: Board -> Int -> IO ()
putBoardline b row = 
  do  putStr (show row)
      putStrLn (": " ++ concat (replicate (b !! (row-1)) "* "))

putBoard :: Board -> IO ()
putBoard b =
  do  putBoardline b 1
      putBoardline b 2
      putBoardline b 3
      putBoardline b 4
      putBoardline b 5


getDigit :: String -> IO Int
getDigit prompt = 
  do  putStr prompt
      x <- getChar
      newline
      if isDigit x then
        return (digitToInt x)
      else
        do  putStrLn "ERROR: Invalid digit"
            getDigit prompt

newline :: IO ()
newline = putChar '\n'


play' :: Board -> Int -> IO ()
play' b p =
  do  putBoard b
      if finished b then
        do  putStr "player "
            putStr (show (next p))
            putStrLn " wins!!"
      else
        do  putStr "player "
            putStr (show p)
            putStrLn "'s turn..."
            row <- getDigit "from which row do you remove stars?: "
            num <- getDigit ("how many stars do you remove from line " ++ show row ++ ": ")
            if valid b row num then
              play' (move b row num) (next p)
              else
              do  putStrLn "ERROR: Invalid operation"
                  play' b p

nim :: IO ()
nim = play' initial 1

type Pos = (Int, Int)
writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs
                
goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


-- ライフ
cls :: IO ()
cls = putStr "\ESC[2J"

--------------- memo ----------------------------
hoge :: IO ()
hoge =
  do  x <- getChar 
      newline
-- これだと getChar のエコーがなくなる．なんで？

fuga :: IO ()
fuga =
  do  x <- getChar
      putChar '5'
-- これはエコーあり

fuga1 :: IO ()
fuga1 = 
  do  x <- getChar 
      putChar '5'
      putChar '\n'
-- これはエコーなし      

fuga2 :: IO ()
fuga2 =
  do  x <- getChar 
      putChar '\n'
      putChar '8'
-- これもエコーなし      
--------------- memo -----------------------------