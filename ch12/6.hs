{-
6.  型 (a -> ) を Monad にしよう
-}

instance Monad ((->) a) where
  -- return :: b -> (a -> b)
  return = pure
  -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  st >>= f = \a -> f (st a) a

{-
  この型を Applicative にする問題は問3で解決済み
  モナド則の型を見る

  1. return x >> f = f x
    x             :: b
    f             :: b -> a -> c
    return x      :: a -> b
    return x >> f :: a -> c

    f x           :: a -> c

  2. mx >>= return = mx
    mx            :: a -> b
    return        :: b -> a -> b
    mx >> return  :: a -> b

  3. (mx >>= f) >> g = mx >>= (\x -> (f x >>= g))
    mx                :: a -> b
    f                 :: b -> a -> c
    mx >>= f          :: a -> c
    g                 :: c -> a -> d
    (mx >>= f) >> g   :: a -> d

    (\x -> (f x >>= g))         :: b -> a -> d
    x                           :: b
    f x                         :: a -> c
    f x >> g                    :: a -> d
    mx >>= (\x -> (f x >>= g))  :: a -> d


-}