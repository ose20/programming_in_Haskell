{-
2.  以下の部分適用された関数の型 (a ->) を関手にしよう
-}

instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> a -> c
  fmap = (.)

