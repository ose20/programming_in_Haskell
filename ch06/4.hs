{-
  4.  2つの非負整数に対する最大公約数を計算する関数 euclid を定義して下さい．
-}

max' m n = if m >= n then m else n
min' m n = if m <= n then m else n

euclid m n  | pos m n && m == n = m
            | pos m n           = euclid (min m n) (max m n - min m n)
  where pos m n = m >= 0 && n >= 0

test1 = euclid 6 27
test2 = euclid 15 90