-- 8.1 type による型宣言

type Pair a = (a, a)

x :: Pair Int
x = (1, 1)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v' | (k', v') <- t, k' == k]


-- 8.4 再帰型

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
          (Node (Leaf 6) 7 (Leaf 9))


-- 8.5 恒真式検査器

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply  (And 
              (Var 'A')
              (Imply (Var 'A') (Var 'B')))
            (Var 'B')

type Env = Assoc Char Bool

eval :: Env -> Prop -> Bool
eval _ (Const b)    = b
eval e (Var b)      = find b e
eval e (Not p)      = not (eval e p)
eval e (And p q)    = eval e p && eval e q
eval e (Imply p q)  = eval e p <= eval e q

vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : (rmdups (filter (/= x) xs))

envs :: Prop -> [Env]
envs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval e p | e <- envs p]


-- 8.7 抽象機械

data Exp = Val Int | Add Exp Exp

type Cont = [Op]
data Op = EVAL Exp | ADD Int

eval' :: Exp -> Cont -> Int
eval' (Val n) cont     = exec cont n
eval' (Add e1 e2) cont = eval' e1 (EVAL e2 : cont)

exec :: Cont -> Int -> Int
exec [] n            = n
exec (EVAL e : c) n  = eval' e (ADD n : c)
exec (ADD n1 : c) n2 = exec c (n1 + n2)

value e = eval' e []

e1 =
  (Add
    (Add
      (Add (Val 1) (Val 2))
      (Add (Val 3) (Val 4))
    )
    (Add (Val 5) (Val 6))
  )

test1 = value e1