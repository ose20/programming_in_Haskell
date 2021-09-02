{-
  7.  以下のカリー化された関数をラムダ式で表してください．

  mult :: Int -> Int -> Int -> Int
  mult x y z = x * y * z
-}

mymult :: Int -> Int -> Int -> Int
mymult = \x -> \y -> \z -> x * y * z

test = mymult 1 2 3