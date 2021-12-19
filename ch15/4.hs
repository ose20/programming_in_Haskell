{-
4.  Implement a function fibs :: [Integer] that generates an infinite list
    of Fibonacci numbers using list comprehension.
-}

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

