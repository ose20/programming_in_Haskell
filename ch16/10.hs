{-
10. Show that List type satisfies Monad rules.

    note.
      instance Functor [] where
        -- fmap :: (a -> b) -> [a] -> [b]
        fmap g xs = [g y | x <- xs]
      instance Applicative [] where
        -- pure :: a -> [a]
        pure x = [x]
        -- (<*>) :: [a->b] -> [a] -> [b]
        fs <*> xs = [f x | f <- fs, x <- xs]
      instance Monad [] where
        (>>=) :: [a] -> (a -> [b]) -> [b]
        xs >>= f = [y | x <- xs, y <- f x]

      Monad rules
        return x >>= f    = f x
        mx >>= return     = mx
        (mx >>= f) >>= g  = mx >>= (\x -> (f x >>= g))
    
    Proof of [return x >>= f = f x]
        return x >>= f
      = [x] >>= f
      = [y | y <- f x]
      = f x
    QED:

    Proof of [mx >>= return = mx]
        mx >>= return
      = [y | x <- mx, y <- return x]
      = [y | x <- mx, y <- [x]]
      = [x | x <- mx]
      = mx
    QED:

    Proof of [(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))]
        (mx >>= f) >>= g
      = [y | x <- mx, y <- f x] >>= g
      = [z | y <- [y | x <- mx, y <- f x], z <- g y]
      = [z | x <- mx, y <- f x, z <- g y]

        mx >>= (\x -> (f x >>= g))
      = [z | x <- mx, z <- (f x >>= g)]
      = [z | x <- mx, z <- [z | y <- f x, z <- g y]]
      = [z | x <- mx, y <- f x, z <- g y]
    QED:

-}