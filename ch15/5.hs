{-
5.  Modify the Prelude functions repeat, take, and replicate to work
    with the following Tree.
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeat' :: a -> Tree a
repeat' x = Node t x t
  where
    t = repeat' x

take' :: Int -> Tree a -> Tree a
take' 0 _             = Leaf
take' _ Leaf          = Leaf
take' n (Node l x r)  = Node (take' (n-1) l) x (take' (n-1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

ex1 = replicate' 2 7