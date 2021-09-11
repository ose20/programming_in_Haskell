{-
9.  抽象機械を拡張して，乗算を扱えるようにしてください．
-}

data Exp = Val Int | Add Exp Exp | Mul Exp Exp
data Op = AEVAL Exp | MEVAL Exp | ADD Int | MUL Int
type Cont = [Op]

eval :: Exp -> Cont -> Int
eval (Val n) c = exec c n
eval (Add e1 e2) c = eval e1 (AEVAL e2 : c)
eval (Mul e1 e2) c = eval e1 (MEVAL e2 : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (AEVAL e : c) n = eval e (ADD n : c)
exec (MEVAL e : c) n = eval e (MUL n : c)
exec (ADD n1 : c) n2 = exec c (n1+n2)
exec (MUL n1 : c) n2 = exec c (n1*n2)

value :: Exp -> Int
value e = eval e []


-- test
e1 = Mul (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))

test1 = value e1