{-
  7.  整列された2つのリストを整列された1つのリストにまとめる関数 merge を定義してください．
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

test1 = merge [1,2,6,12,56,66] [-2,4,5,20,21,36,100]