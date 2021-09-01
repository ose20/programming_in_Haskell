-- 2.5.3
a = b + c
  where
    b = 1
    c = 1

d = a * 2

-- 2.5.5
-- Factorial of a positive integer:
factorial n = product  [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

{- 
double x = x + x

quadruple x =  double (double x)
-}