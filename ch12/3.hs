{-
3.  型 (a ->) を Applicative にしよう
-}


instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure x = \y -> x
  -- <*> :: (a -> (b -> c)) -> (a -> b) -> a -> c
  x <*> y = \z -> x z (y z)
  