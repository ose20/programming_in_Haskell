{-
  2.  プレリュードでの定義を見ずに以下の高階関数を定義してください．
-}

fold_left :: (a -> b -> a) -> a -> [b] -> a
fold_left f v []      = v
fold_left f v (x:xs)  = fold_left f (f v x) xs

-- a. リストのすべての要素が述語を満たすか検査する関数
all' :: (a -> Bool) -> [a] -> Bool
all' p = fold_left (\s x -> s && p x) True

test1 = all' even [1..10]
test2 = all' even [2, 4, 6]

-- b. リストのすべての要素が述語を満たすか検査する関数
any' :: (a -> Bool) -> [a] -> Bool
any' p = fold_left (\s x -> s || p x) False

test3 = any' even [1..10]
test4 = any' even [1,3,5]

-- c. リストの先頭から述語を満たす連続した要素を取り出す関数
takewhile :: (a -> Bool) -> [a] -> [a]
takewhile p []                  = []
takewhile p (x:xs)  | p x       = x : takewhile p xs
                    | otherwise = []

test5 = takewhile even [2, 4, 6, 12, 11, 8, 20]
test6 = takewhile even [1..10]

-- d. リストの先頭から述語を満たす連続した要素を取り除く関数
dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile p []                  = []
dropwhile p (x:xs)  | p x       = dropwhile p xs
                    | otherwise = (x:xs)

test7 = dropwhile even [2, 4, 6, 1, 2, 4]
test8 = dropwhile even [1..10]