{-
  1. 二分木を Functor にしよう
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{-
      関手則
  fmap id     = id
  fmap (g. h) = fmap g. fmap h
-}