{-
5.  Suppose the following definitions are given.
      take 0 _      = []
      take _ []     = []
      take n (x:xs) = x : take (n-1) xs

      drop 0 xs     = xs
      drop _ []     = []
      drop n (_:xs) = drop (n-1) xs
    Using these definitions and definition of ++, prove
      take n xs ++ drop n xs = xs 
    using mathematical induction on the integer n >= 0
    and structual induction on the list xs simultaneously.

    note.
      [] ++ ys = ys
      (x:xs) ++ ys = x : (xs ++ ys)
      xs ++ [] = xs
      xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

    Proof:
    --- case: n = 0
      take n xs ++ drop n xs
    = [] ++ xs
    = xs
    
    --- case: xs = []
      take n xs ++ drop n xs
    = [] ++ []
    = []
    = xs

    --- case: n = m+1(m >= 0), xs = y:ys
      take n xs ++ drop n xs
    = take (m+1) (y:ys) ++ drop (m+1) (y:ys)
    = y : (take m ys) ++ (drop m ys)
    = y : ys
    = xs
-}