{-
  2.  プレリュード関数 compare を使って探索木用の関数 occurs :: Ord a => a -> Tree a -> Bool を再定義してください．
-}

data Tree a = Leaf | Node a (Tree a) (Tree a)

add :: Ord a => a -> Tree a -> Tree a
add x Leaf = Node x Leaf Leaf
add x (Node y left right)  =
  case compare x y of
    EQ -> Node y left right
    LT -> Node y (add x left) right
    GT -> Node y left (add x right)

occurs :: Ord a => a -> Tree a -> Bool
occurs x Leaf = False
occurs x (Node y left right) =
  case compare x y of
    EQ -> True
    LT -> occurs x left
    GT -> occurs x right

-- test
t1 = add 10 (add 2 (add 4 (add 8 (add (-1) (add 3 Leaf)))))

test1 = occurs 1 t1
test2 = occurs 14 t1
test3 = occurs 8 t1