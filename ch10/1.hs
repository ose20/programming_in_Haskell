{-
1.  リスト内包表記とプレリュード関数 sequence_ :: [IO a] -> IO () を用いて
    putStr :: String -> IO () を再定義してください．
-}

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar c | c <- s]

---------- test begin -------------
test1 :: IO ()                   --
test1 = putStr' "hello world"    --
test2 :: IO ()                   --
test2 = putStr' "hello world\n"  --
---------- test end --------------- 
