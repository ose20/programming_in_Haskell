{-
  2.  リストの3つ目の要素を返す関数 third :: [a] -> a を，以下のそれぞれの方法で定義してください．
-}

-- head と tail を使う
third1 xs = head (tail (tail xs))

test1 = third1 [1..10]

-- リストのインデックス演算子 !! を使う
third2 xs = xs !! 2

test2 = third2 [1..10]

-- パターンマッチを使う
third3 (_ : _ : x : _) = x

test3 = third3 [1..10]