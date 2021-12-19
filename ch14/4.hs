import Data.Foldable

{-
4.  The following binary trees that stores data in a clause should be
    an instance of Foldable and Traversable.
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show


instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

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

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf         = pure Leaf
  traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r

