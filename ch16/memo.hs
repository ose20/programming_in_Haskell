data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m      = m
add (Succ n) m  = Succ (add n m)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten t = flatten' t []

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) xs = n:xs
flatten' (Node l r) xs = flatten' l (flatten' r xs)

tree1 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
test1 = flatten tree1

-- 16.7 コンパイラの正しさ
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD
          deriving Show

exec :: Code -> Stack -> Stack
exec [] s                   = s
exec (PUSH n : c) s         = exec c (n : s)
exec (ADD : c) (m : n : s)  = exec c (n+m : s)

{- 
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]
-}

{-
証明したい性質: 任意の e::Exp に対して
exec (comp e) s = eval e : s

--- case: e = Val n
  exec (comp (Val n)) s
=
  exec [PUSH n] s
= 
  exec [] (n:s)
=
  n:s
=
  eval (Val n) : s

--- case: e = Add x y
  exec (comp (Add x y)) s 
=
  exec (comp x ++ comp y ++ [ADD]) s
= (++ の結合則)
  exec (comp x ++ (comp y ++ [ADD])) s
= (++ の分配則　後述)
  exec (comp y ++ [ADD]) (exec (comp x) s)
=
  exec (comp y ++ [ADD]) (eval x : s)
= (++ の分配則)
  exec [ADD] (exec (comp y) (eval x : s))
=
  exec [ADD] (eval y : eval x : s)
=
  eval x + eval y : s
=
  eval (Add x y) : s
=
  eval (Add e1 e2) : s
:QED

++ の分配則: 
exec (c ++ d) s = exec d (exec c s)

証明:cの構造に関する帰納法
--- case: c = []
  exec ([] ++ d) s
= exec d s
= exec d (exec [] s)

--- case: c = PUSH n : c
  exec ((PUSH n : c) ++ d) s
= exec (c ++ d) (n:s)
= exec d (exec c (n:s))
= exec d (exec (PUSH n : c) s)

--- case c = ADD : c
  exec (ADD : c ++ d) s
= exec (ADD : c ++ d) (m : n : s')    -- s がこの形であると仮定する（実際 comp の定義を見れば，comp によって作られる Stack では ADD の前に必ず要素が二つはある）
= exec (c ++ d) (n+m : s')
= exec d (exec c (n+m : s'))
= exec d (exec (ADD : c) (m : n : s'))

-}


-- comp' e c = comp e ++ c を満たす comp' を探す
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp :: Expr -> Code
comp e = comp' e []

{-
これを用いて，コンパイラの正しさを表す等式は以下のようにかける．

exec (comp' e c) s = exec c (eval e : s)
これは s = c = [] とすると次のように特殊化できる
exec (comp' e []) [] = [eval e]

証明: e の構造に関する帰納法による
--- case: e = Val n
  exec (comp' (Val n) c) s 
= exec (PUSH n : c) s
= exec c (n : s)
= exec c (eval (Val n) : s)

--- case: e = Add x y
  exec (comp' (Add x y) c) s 
= exec (comp' x (comp' y (ADD : c))) s
= exec (comp' y (ADD : c)) (eval x : s)
= exec (ADD : c) (eval y : eval x : s)
= exec c (eval x + eval y : s)
= exec c (eval (Add x y) : s)
-}