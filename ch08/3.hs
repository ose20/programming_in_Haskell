{-
  3.  二分木が平衡しているかを判定する関数　balanced :: Tree a -> Bool を定義してください．
      二分木が平衡しているとは，すべての節において，左部分木と右部分木の葉の数の差が高々1であることである．
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

numOfLeaf :: Tree a -> Int
numOfLeaf (Leaf x)          = 1
numOfLeaf (Node left right) = numOfLeaf left + numOfLeaf right

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node left right) 
  | abs (numOfLeaf left - numOfLeaf right) <= 1 = balanced left && balanced right
  | otherwise                                   = False


-- test
t1 =
  Node 
    (Node
      (Leaf 1)
      (Leaf 1)  
    )
    (Leaf 1)

t2 =
  Node
    (Node
      (Node
        (Leaf 1)
        (Leaf 1)
      )
      (Leaf 1))
    (Leaf 1)

test1 = balanced t1
test2 = balanced t2