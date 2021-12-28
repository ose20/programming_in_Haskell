{-
4.  Suppose that the following definitions are given.  
      [] ++ ys      = ys
      (x:xs) ++ ys  = x : (xs ++ ys)
    Prove the following two properties by structual induction on xs.
      xs ++ []          = xs
      xs ++ (ys ++ zs)  = (xs ++ ys) ++ zs

    note:
      [] ++ ys = ys
      (x:xs) ++ ys = x : (xs ++ ys)

    Proof of xs ++ [] = xs :
    ---case xs = []
      [] ++ [] = []
    --- case xs = x:xs
      x:xs ++ [] 
    = x : (xs ++ [])
    = x : xs

    Proof of xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
    --- case xs = []
      [] ++ (ys ++ zs)
    = ys ++ zs
    = ([] ++ ys) ++ zs
    --- case xs = x:xs
      (x:xs) ++ (ys ++ zs) 
    = x : (xs ++ (ys ++ zs))
    = x : ((xs ++ ys) ++ zs)
    = (x : (xs ++ ys)) ++ zs
    = (x:xs ++ ys) ++ zs

-}