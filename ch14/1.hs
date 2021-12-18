{-
1.  The following is part of the difinition in Data.Monoid.
    Please complete the difinition to make the pair a Monoid.
-}

{-

instance (Monoid a, Monoid b) => Monoid (a, b) where
  -- mempty :: (a, b)
  mempty = (mempty, mempty)
  -- mappend :: (a,b) -> (a,b) -> (a,b)
  mappend (a1,b1) (a2,b2) = (a1 `mappend` a2, b1 `mappend` b2)

-}