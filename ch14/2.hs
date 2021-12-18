{-
2.  Show how to make a function of type a -> b a monoid.
    However, assume that the result-type b is a monoid.
-}

{-

instance Monoid b => Monoid (a -> b) where
  -- mempty :: (a -> b)
  mempty = const mempty
  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = \x -> f x `mappend` g x

-}  