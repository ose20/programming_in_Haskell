{-
2.  putBoard :: Board -> IO () が表示できるボードは列が5つに固定されていましたが，任意の大きさのボードを表示できるようにして食うださい．
-}
import System.IO
import Data.Char
import System.Console.Haskeline (Completion(isFinished))

data Player = Alice | Bob
instance Show Player where
  show Alice = "Alice"
  show Bob = "Bob"

next :: Player -> Player
next Alice = Bob
next Bob = Alice  

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid b row num = num <= b !! (row-1)

move :: Board -> Int -> Int -> Board
move b r n = [update row num | (row, num) <- zip [1..] b]
  where update row num = if row == r then num-n else num


putBoard :: Board -> IO ()
putBoard b = aux b 1
  where
    aux [] _ = return ()
    aux (x:xs) n =
      do  putStr (show n ++ ":")
          putStrLn (concat (replicate (b !! (n-1)) " *"))
          aux xs (n+1)
    
---------- test begin ----------
test1 = putBoard [1..3]
test2 = putBoard [1..5]
test3 = putBoard [1,2,3,4,3,2,1]
---------- test end ------------

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
        do  putStrLn "ERROR: Invalid digit"
            getDigit prompt


play :: Board -> Player -> IO ()
play b p =
  do  putBoard b
      if finished b then
        do  putStr (show p)
            putStrLn " wins!!"
      else
        do  putStr (show p)
            putStrLn "'s turn..."
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove :"
            if valid b row num then
              play (move b row num) (next p)
            else
              do  putStrLn "ERROR: invalid operation"
                  play b p
        
nim :: IO ()
nim = play initial Alice
