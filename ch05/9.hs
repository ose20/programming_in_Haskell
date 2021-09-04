{-
  9.  内積を，リスト内包表記によって定義してください．
-}

scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys]
test1 = scalarproduct [1,2,3] [4,5,6]