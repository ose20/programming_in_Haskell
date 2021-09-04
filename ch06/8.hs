{-
  8.  merge を使ってマージソートを実行する関数 msort を定義してください．
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x] -- これがないと関数が止まらないヨ
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

test1 = msort "jfeowitjueijtu4rfj40urju309"
test2 = msort [1,65,43,6,1,7,5,34,6,34,6,86,443,64,5,5]