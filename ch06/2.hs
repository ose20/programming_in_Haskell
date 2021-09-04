{-
  2.  与えられた非負整数から 0 までの和を計算する関数 sumdown を定義してください．
-}

sumdown n | n == 0  = 0
          | n > 0   = n + sumdown (n-1)

test1 = sumdown 10
-- test2 = sumdown (-10)          