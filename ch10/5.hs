{-
5.  関数 sequence :: [IO a] -> IO [a] は，アクションのリストを実行した後，
    結果の値をリストとして返します．この関数を用いて adder を再実装してください．
-}

getone :: IO Int
getone = do
  x <- getLine
  return (read x :: Int)

adder :: IO ()
adder = do
  putStr "How many numbers? "
  x <- getLine
  lst <- sequence [getone | _ <- [1..(read x :: Int)]]
  putStrLn ("The total is " ++ show (sum lst))