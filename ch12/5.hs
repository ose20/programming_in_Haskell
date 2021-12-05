{-
5.  4つのアプリカティブ則に出てくる変数の型を調べよう
-}

{-

pure :: a -> A a
<*>  :: A (a -> b) -> A a -> A b

1. pure id <*> x = x
  id :: a -> a
  x = A a

2. pure (g x) = pure g <*> pure x
  g :: a -> b
  x :: a
  pure (g x) :: A b

  pure g :: A (a -> b)
  pure x :: A a
  pure g <*> pure x :: A b

3. x <*> pure y = pure (\g -> g y) <*> x
  x :: A (a -> b)
  y :: a
  x <*> pure y :: A b

  pure (\g -> g y) :: A ((a -> b) -> b)

4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
  x :: A (b -> c)
  y :: A (a -> b)
  z :: A a
  x <*> (y <*> z) = A c

  pure (.) :: A ((b -> c) -> (a -> b) -> a -> c)
  pure (.) <*> x <*> y :: A (a -> c)

-}