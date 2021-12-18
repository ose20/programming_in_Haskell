import Data.Monoid
import Data.Foldable
import GhcPrelude (Foldable)
import PrelNames (traversableClassKey)


test1 = mconcat [Sum 2, Sum 3, Sum 4]
test2 = mconcat [Product 2, Product 3, Product 4]
test3 = getSum (foldMap Sum [1..10])

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x)   = x
  fold (Node l r) = fold l `mappend` fold r
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x)    = f x
  foldMap f (Node l r)  = foldMap f l `mappend` foldMap f r
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x)    = f x v
  foldr f v (Node l r)  = foldr f (foldr f v r) l
  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x)    = f v x
  foldl f v (Node l r)  = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
test4 = foldr (+) 0 tree

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

test5 = average tree

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)

test6 = Main.and [True, False, True]
test7 = Main.or (Node (Leaf True) (Leaf False))
test8 = Main.all even [1,2,3]
test9 = Main.any even (Node (Leaf 1) (Leaf 2))

concat :: Foldable t => t [a] -> [a]
concat = fold

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r) 

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = Leaf <$> g x
  traverse g (Node l r) =
    Node <$> traverse g l <*> traverse g r