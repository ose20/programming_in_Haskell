{-
9.  altMap :: (a -> b) -> (a -> b) -> [a] -> [b] を定義してください．
-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = aux True xs
  where
    aux _ []          = []
    aux True (x:xs)   = f1 x : aux False xs
    aux False (x:xs)  = f2 x : aux True xs

test1 = altMap (+ 10) (* 10) [1..10]