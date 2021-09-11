{-
  1.  関数 add と同様に，自然数の乗算 mult :: Nat -> Nat -> Nat を再帰的に定義してください．
-}

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n        = n
add (Succ n1) n2  = add n1 (Succ n2)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

make :: Int -> Nat
make 0          = Zero
make n | n > 0  = Succ (make (n-1))

nat2int :: Nat -> Int
nat2int Zero      = 0
nat2int (Succ n)  = nat2int n + 1

test1 = nat2int (mult (make 2) (make 3))
test2 = nat2int (mult (make 100) (make 1000))