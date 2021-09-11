{-
  4.  foldl を用いて，十進表記を整数に変換する関数 dec2int を定義してください．
-}

dec2int :: [Int] -> Int
dec2int = foldl (\seed x -> seed * 10 + x) 0

test1 = dec2int [1,8,0,7]
test2 = dec2int []
test3 = dec2int [0]