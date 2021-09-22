{-
4.  指定した数だけキーボードから数字を読み取り，それらの和を表示する adder :: IO () を定義してください．
-}

newline :: IO ()
newline = putChar '\n'

iter :: Int -> Int -> IO ()
iter 0 res = putStrLn ("The toal is " ++ show res)
iter i res = 
  do  x <- getLine
      iter (i-1) (res + (read x :: Int))

adder :: IO ()
adder =
  do  putStr "How many numbers? "
      n <- getLine
      iter (read n :: Int) 0  