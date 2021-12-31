data Expr = Val Int | Add Expr Expr
type Stack = [Int]
type Cont = Stack -> Stack

add :: Stack -> Stack
add (y:x:s) = x+y:s
add _ = []

push :: Int -> Stack -> Stack
push n s = n : s


eval :: Expr -> Int
eval e = head (eval' e [])


--- 17.5 脱高階関数
-- 継続を太らせていくイメージ
haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c    = pushC n c
eval'' (Add x y) c  = eval'' x (eval'' y (addC c))

eval' :: Expr -> Cont
eval' e = eval'' e haltC

data Code = HALT | PUSH Int Code | ADD Code
            deriving Show

exec :: Code -> Stack -> Stack
exec HALT s           = s
exec (PUSH n c) s     = exec c (n:s)
exec (ADD c) (m:n:s)  = exec c (m+n:s)

comp'' :: Expr -> Code -> Code
comp'' (Val n) c    = PUSH n c
comp'' (Add x y) c  = comp'' x (comp'' y (ADD c))

comp' :: Expr -> Code
comp' e = comp'' e HALT

{-
  comp' (Add (Val 1) (Val 2)) = comp'' (Add (Val 1) (Val 2)) HALT = comp'' (Val 1) (comp'' (Val 2) (ADD HALT))
  = PUSH 1 (comp'' (Val 2) (ADD HALT)) = PUSH 1 (PUSH 2 (ADD HALT))

  exec (PUSH 1 (PUSH 2 (ADD HALT))) [] = exec (PUSH 2 (ADD HALT)) [1] = exec (ADD HALT) [2,1]
  exec HALT [3] = [3]
  
-}

{-
  - Specifications:
    exec (comp e) s     = eval e : s
    exec (comp' e c) s  = exec c (eval e : s)
  
  - First, we want to define comp'. To do so, we find some c'::Code such that exec (comp' e c) s = exec c' s
    --- case: e = Val n
      exec (comp' (Val n) c) s
    = exec c (eval (Val n) : s)
    = exec c (n : s)
      - requirement for exec
        exec c (n:s) = exec c' s (find proper c'!)
          define PUSH :: Int -> Code -> Code
        exec (PUSH n c) s = exec c (n:s)
    = exec (PUSH n c) s
    --- case: e = Add x y
      exec (comp' (Add x y) c) s
    = exec c (eval (Add x y) : s)
    = exec c (eval x + eval y : s)
      - note: Structural induction allows us to make the following assumptions
        exec (comp' x c) s = exec c (eval x : s)
        exec (comp' y c) s = exec c (eval y : s)
      - requirement
        exec c' (eval y : eval x : s) = exec c (eval x + eval y : s)
        - generalize
          exec c' (m : n : s) = exec c (n+m : s)
            - define: ADD :: Code -> Code
          exec (ADD c) (m : n : s) = exec c (n+m : s)
        exec (ADD c) (eval y : eval x : s) = exec c (eval x + eval y : s)
      exec (ADD c) (eval y : eval x : s)
    = (assumption on y)
      exec (comp' y (ADD c)) (eval x : s)
    = (assumption on x)
      exec (comp' x (comp' y (ADD c))) s

    From the above, we have the definition of comp'.
      comp' (Val n) c = PUSH n c
      comp' (Add x y) c = comp' x (comp' y (ADD c))

    then, newly defined constructors are as follows.
      data Code = PUSH Int Code | ADD Code

  - Next, we drive the definition of comp. Specifically, we find a term c of Code type such that exec (comp e) s = exec c s.
    Then we can conclude that comp e = c.
    --- case: e = Val n
      specification: 
        exec (comp e) s     = eval e : s
        exec (comp' e c) s  = exec c (eval e : s)
      exec (comp (Val n) s) = eval (Val n) : s
      = exec HALT (eval (Val n) : s)  --- new def: exec HALT s = s, Code += HALT
      = exec (comp' (Val n) HALT) s --- carefully observation shows that replacing Val n with Add x y is not a problem.
      
    Therefore:
      comp e = comp' e HALT
      exec HALT s = s
      Code += HALT

  Finally, we drived the full definition.
    data Code = HALT | PUSH Int Code | ADD Code

    comp :: Expr -> Code
    comp e = comp' e HALT

    comp' :: Expr -> 
    comp' (Val n) c   = PUSH n c
    comp' (Add x y) c = comp' x (comp' y (ADD c))

    exec :: Code -> Stack -> Stack
    exec HALT s = s
    exec (PUSH n c) s = exec c (n:s)
    exec (ADD c) (m : n : s) = exec c (n+m : s)

-}


{-
  exec :: Code -> Cont
  exec HALT       = haltC
  exec (PUSH n c) = pushC n (exec c)
  exec (ADD c)    = addC (exec c)

  - exec HALT s = haltC = s
  - exec (PUSH n c) s = pushC n (exec c) s = ((exec c) . push n) s = exec c (n:s)
  - exec (ADD c) s = addC (exec c) s = (exec c . add) s = (exec c . add) (m:n:s')
    = exec c (m+n:s')
-}


{-    :memo:
  eval' (Add (Val 1) (Val 2) [])
= eval'' (Add (Val 1) (Val 2)) id []
= eval'' (Val 1) (eval'' (Val 2) (id . add) [])
= eval'' (Val 2) (id . add) (push 1 [])
= (id . add) (push 2 (push 1 []))
= (id . add) (2 : 1 : [])
= id (1+2)
= 1+2
= 3
-}