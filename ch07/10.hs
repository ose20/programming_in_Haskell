{-
10. 第4章に出てきた Luhn アルゴリズムを実装する関数を，任意の長さのカード番号を取り扱えるように改良してください．
-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = aux True xs
  where
    aux _ []          = []
    aux True (x:xs)   = f1 x : aux False xs
    aux False (x:xs)  = f2 x : aux True xs

luhnDouble:: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y
  where y = 2 * x

luhn :: [Int] -> Bool
luhn xs = s `mod` 10 == 0
  where
    s = sum (altMap id luhnDouble (reverse xs))

test1 = luhn [1, 7, 8, 4]
test2 = luhn [4, 7, 8, 3]
test3 = luhn [1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8]