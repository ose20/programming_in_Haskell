{-
6.  Implement Newton's method for finding the square root
    of a floating point number n.
-}

sqroot :: Double -> Double
sqroot x = snd . head $ dropWhile p (zip it (tail it))
  where
    err = 0.00001
    it = iterate next 1 
    next a = (a + x/a) / 2
    p (x, y) = abs (x - y) > err

ex1 = sqroot 4
ex2 = sqroot 5
ex3 = sqroot 100