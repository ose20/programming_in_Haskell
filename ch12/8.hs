{-
8.  ST 型を，Monad のインスタンスの定義を利用して Functor, Applicative のインスタンスにしてみよう
-}

type State = Int 
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do  x <- st
                  return (g x)
{-
  fmap g st = st >>= \x -> return (g x)
-}

instance Applicative ST where
  -- pure :: a -> ST a
  pure a = S(\s -> (a,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do  f <- stf
                    x <- stx
                    return (f x)
{-
  stf <*> stx = stf >>= (\f ->
                stx >>= (\x ->
                return f x))
-}

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S(\s ->
    let (a,s') = app st s in app (f a) s')
