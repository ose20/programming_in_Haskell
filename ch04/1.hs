{-
  1.  プレリュード関数を使って，長さが偶数のリストを半分に分割する関数
      halve :: [a] -> ([a], [a]) を定義してください．
-}

halve xs = (take n xs, drop n xs)
            where
              n = length xs `div` 2

test1 = halve [1..6]
test2 = halve [1..20]