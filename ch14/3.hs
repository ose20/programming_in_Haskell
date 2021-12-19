import Data.Foldable
{-
3.  Make Maybe an instance of Foldable and Traversable. However,
    do not rely of the default definitions, but define fold, foldMap,
    foldr, foldl and traverse.
-}

data Maybe' a = Nothing' | Just' a
  deriving Show

instance Foldable Maybe' where
  -- fold :: Monoid a => Maybe' a -> a
  fold Nothing'   = mempty
  fold (Just' x)  = x
  -- foldMap :: Monoid b => (a -> b) -> Maybe' a -> b
  foldMap g Nothing'  = mempty
  foldMap g (Just' x) = g x
  -- foldr :: (a -> b -> b) -> b -> Maybe' a -> b
  foldr _ v Nothing'  = v -- type b is not necessarily a Monoid, so we can't use mempty
  foldr f v (Just' x) = f x v
  -- foldl :: (a -> b -> a) -> a -> Maybe' b -> a
  foldl _ v Nothing'  = v
  foldl f v (Just' x) = f v x

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b  
  fmap _ Nothing'   = Nothing'
  fmap g (Just' x)  = Just' (g x)

instance Traversable Maybe' where
  -- traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse g Nothing'   = pure Nothing'
  traverse g (Just' x)  = Just' <$> g x

ex1 = traverse (: []) (Just' 3)


