import Data.Foldable
{-
5.  Use foldMap to generalize the higher-order function filter for lists
    so that it can used for any instance of Foldable.
-}

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = filter p . toList

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show


instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf         = mempty
  fold (Node l x r) = x `mappend` fold l `mappend` fold r
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap g Leaf          = mempty
  foldMap g (Node l x r)  = g x `mappend` foldMap g l `mappend` foldMap g r
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v Leaf          = v
  foldr f v (Node l x r)  =  foldr f (f x (foldr f v r)) l
  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v Leaf          = v
  foldl f v (Node l x r)  = foldl f (f (foldl f v l) x) r

tree = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)
ex1 = filterF even tree