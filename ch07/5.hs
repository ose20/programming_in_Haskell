{-
  5.  プレリュードの定義を見ないで以下の2つの高階関数を定義してください．
-}

-- a. 「引数に組をとる関数」を「カリー化された関数」へ変換する関数 curry 
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

plus (a, b) = a + b
test1 = curry' plus 10 20

-- b. 「引数が2つのカリー化された関数」を「引数を組を取る関数」へ変換する関数 uncurry
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

curry_plus a b = a + b
test2 = uncurry curry_plus (10, 100)