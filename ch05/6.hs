{-
  6.  自分自身を除く約数の和が自分自身と等しい時，その整数を完全数という．
      与えられた引数以下の完全数を列挙する関数 perfects を，
      リスト内包表記と関数 factors を使って定義してください．
-}

factors n = [x | x <- [1..n], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]
test1 = perfects 500