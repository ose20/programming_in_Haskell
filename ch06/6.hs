{-
  6.  プレリュード関数を見ないで，以下のプレリュード関数を再帰を使って定義してください．
-}

-- a. リストの要素がすべて True であるか検査する関数
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

test1 = and' []
test2 = and' [True, True, False, True, True]
test3 = and' [True]
test4 = and' [False]
test5 = and' [False, False]

-- b. リストのリストを受け取り，要素であるリストを連結する関数
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

test6 = concat' [['a', 'b', 'c'], ['d', 'e', 'z'], ['f', 'g']]
test7 = concat' ["I ", "am ", "a ", "HERO!"]
test8 = concat' [[1,2,3,4], [10, 20, 30]]

-- c. 指定された要素を n 個もつリストを生成する関数
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

test9 = replicate' 10 1
test10 = replicate' 5 'a'

-- d. 空でないリストの n 番目の要素を取り出す関数
(!!-) :: [a] -> Int -> a
(x:xs) !!- 0 = x
(x:xs) !!- n = xs !!- (n-1)

test11 = [1..10] !!- 0
test12 = [1..10] !!- 8

-- e. リストの要素に含まれるか検査する関数
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) | x == y    = True
              | otherwise = elem x ys

test13 = elem' 2 [1..10]
test14 = elem' 30 [1..10]              