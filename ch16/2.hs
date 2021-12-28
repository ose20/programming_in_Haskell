{-
2.  Prove that addition satisfies the commutative law; add n m = add m n.
    However, you may use the result of Problem 1 and add n Zero = n.
-}

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m      = m
add (Succ n) m  = Succ (add n m)

{-
First, we need to prove that add n Zero = n by induction on n.
-- case: n = Zero
add Zero Zero = Zero

-- case: n = Succ n
  add (Succ n) Zero 
= Succ (add n Zero)

Then, The result of problem 1 is as follows.
  add n (Succ m) = Succ (add n m)

proposition: add n m = add m n.
proof: by induction on n
--- case: n = Zero
  add Zero m 
= m
= add m Zero

--- case: n = Succ n
  add (Succ n) m
= Succ (add n m)
= Succ (add m n)
= add m (Succ n)

-}