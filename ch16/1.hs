{-
1.  Prove that add n (Succ m) = Succ (add n m) by induction on n.  
-}

data Nat = Zero | Succ Nat
add :: Nat -> Nat -> Nat
add Zero m      = m
add (Succ n) m  = Succ (add n m) 

{-
--- case: n = Zero
  add Zero (Succ m) 
= Succ m
= Succ (Add Zero m)

--- case: n = Succ n
  add (Succ n) (Succ m) 
= Succ (add n (Succ m))
= Succ (Succ (add n m))
= Succ (Add (Succ n) m)

-}