{-
3.  The prelude funtion all is defined as follows

    all :: (a -> Bool) -> [a] -> Bool
    all p []      = True
    all p (x:xs)  = p x && all p xs

    Based on the above, prove the following

    all (== x) (replicate n x) = True

    provided that

    replicate :: Int -> a -> [a]
    replicate 0 _ = []
    replicate n x = x : replicate (n-1) x


proof:
--- case: n = 0
  all (== x) (replicate 0 x)
= all (== x) []
= True

--- case: n >= 1
  all (== x) (replicate n x)
= all (== x) (x : replicate (n-1) x)
= x == x && all (== x) (replicate (n-1) x)
= True

-}



