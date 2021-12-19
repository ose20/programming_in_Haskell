
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
sieve [] = []

{-
sieve [2..]
---> 2 : sieve [x | x <- xs, x `mod` 2 /= 0]  -- (L2 = [x | x <- xs, x `mod` 2 /= 0])
---> 2 : sieve [3, (L2[1:])]
---> 2 : 3 : sieve [x | x <- (L2[1:]), x `mod` 3 /= 0] -- (L3 = [x <- (L2[1:]), x `mod` 3 /= 0])
---> 2 : 3 : sieve [5, (L3[1:])]
---> 2 : 3 : 5 : sieve [x | x <- L3[1:], x `mod` 5 /= 0]
-}