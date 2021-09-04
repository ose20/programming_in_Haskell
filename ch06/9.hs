{-
  9.  以下のプレリュード関数を定義してください
-}

-- a. 数値のリストに対して，その要素の和を計算する関数 sum
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

test1 = sum [1,2,3,4,5]

-- b. リストの先頭から n この要素を取り出す関数 take
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : (take' (n-1) xs)

test2 = take 0 [1..10]
test3 = take 3 [1..10]
test4 = take 20 [1..10]

-- c. 空でないリストの末尾の要素を取り出す関数 last
last' :: [a] -> a
last' [x] = x
last' (x1:x2:xs) = last' (x2:xs)

test5 = last' [1..10]
test6 = last' "abcdefg"