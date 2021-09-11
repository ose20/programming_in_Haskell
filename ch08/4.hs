{-
4.  空でないリストを，平衡木に変換する関数 balance :: [a] -> Tree a を定義してください．
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

half :: [a] -> ([a], [a])
half xs = (take n xs, drop n xs)
  where n = length xs `div` 2

balance :: [a] -> Tree a
balance xs
  | l == 1    = Leaf (head xs)
  | l > 1     = Node (balance left) (balance right)
      where 
        (left, right) = half xs
        l = length xs