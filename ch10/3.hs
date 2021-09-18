{-
3.  問題1と同じように，リスト内包表記とsequence_を使って，putBoardの拡張版を再定義してください．
-}

import System.IO
import Data.Char

data Player = Alice | Bob
instance Show Player where
  show Alice = "Alice"
  show Bob = "Bob"

next :: Player -> Player
next Alice = Bob
next Bob = Alice  

type Board = [Int]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b row num = num <= b !! (row-1)

initial :: Board
initial = [5,4,3,2,1]

move :: Board -> Int -> Int -> Board
move b r n = [update row num | (row, num) <- zip [1..] b]
  where update row num = if row==r then num-n else num

putBoard :: Board -> IO ()
putBoard b = sequence_ [putline c | c <- [1..(length b)]]
  where
    putline c = do  putStr (show c)
                    putStrLn (":" ++ concat (replicate (b!!(c-1)) " *"))

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt =
  do  putStr prompt
      x <- getChar
      newline
      if isDigit x then
        return (digitToInt x)
      else
        do  putStrLn "ERROR: invalid digit"
            getDigit prompt

play :: Board -> Player -> IO ()
play b p =
  do  putBoard b
      if finished b then
        do  putStr (show (next p))
            putStrLn " wins!!"
      else
        do  putStr (show p)
            putStrLn "'s turn..."
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove: "
            if valid b row num then
              play (move b row num) (next p)
            else 
              do  putStrLn "ERROR: invalid operation"
                  play b p

nim :: IO ()
nim = play initial Alice