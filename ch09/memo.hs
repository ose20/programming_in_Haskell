data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

app :: Op -> Int -> Int -> Int
app Add x y = x + y
app Sub x y = x - y
app Mul x y = x * y
app Div x y = x `div` y

data Exp = Val Int | App Op Exp Exp

instance Show Exp where
  show (Val n) = show n
  show (App o l r) = aux l ++ show o ++ aux r
    where
      aux (Val n) = show n
      aux e = "(" ++ show e ++ ")"

eval :: Exp -> [Int]
eval (Val n) = [n]
eval (App o l r) =
  [app o x y |  x <- eval l,
                y <- eval r,
                valid o x y]


e1 = App Add (Val 2) (App Mul (Val 3) (Val 5))
test1 = eval e1
test2 = show e1


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

test3 = subs [1..3]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

test4 = interleave 1 [2..4]

perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concat (map (interleave x) (perm xs))

test5 = perm [1..3]


choices :: [a] -> [[a]]
choices = concat . (map perm) . subs

test6 = choices [1..3]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

test7 = split [1..4]

{-
exps : 与えられた整数リストを，並べられた順番で使ってできる Exp を全て返す
-}
exps :: [Int] -> [Exp]
exps [] = []
exps [v] = [Val v]
exps xs = [e |  (ls, rs) <- split xs,
                lexp <- exps ls,
                rexp <- exps rs,
                e <- combine lexp rexp]

{-
combine : 2つの Exp をオペランドとする Exp としてありうるものをすべて返す．
-}
combine :: Exp -> Exp -> [Exp]
combine l r = [App op l r | op <- ops]
  where ops = [Add, Sub, Mul, Div]

test8 = exps [1, 2]
test9 = exps [1..3]


{-
solutions : 使える整数のリストと目標とする整数から，それを達成する式の一覧を計算する．
-}
solutions :: [Int] -> Int -> [Exp]
solutions xs goal = [ exp | choice <- choices xs,
                            exp <- exps choice,
                            eval exp == [goal]]

test10 = solutions [1,3,7,10,25,50] 765

-- main :: IO ()
-- main = print test10

{-
高速化
-}

{-
与えられたリストとから，valid な Exp だけ生成するようにする．
-}
validexp :: [Int] -> [(Exp, Int)]
validexp [] = []
validexp [n] = [(Val n, n)]
validexp xs = [(exp, val) | (ls, rs) <- split xs,
                            lexp <- validexp ls,
                            rexp <- validexp rs,
                            (exp, val) <- combine' lexp rexp]

{-
combine' : 2引数に対して，それを使った valid な Exp だけリストにして返す．
-}
combine' :: (Exp, Int) -> (Exp, Int) -> [(Exp, Int)]
combine' (lexp, lval) (rexp, rval) = [(App op lexp rexp, app op lval rval) | op <- ops, valid op lval rval]
  where ops = [Add, Sub, Mul, Div]

solutions' :: [Int] -> Int -> [Exp]
solutions' xs goal = [ans |  choice <- choices xs,
                            (ans, val) <- validexp choice,
                            val == goal]

-- main :: IO ()
-- main = print (solutions' [1,3,7,10,25,50] 765)

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

combine'' :: (Exp, Int) -> (Exp, Int) -> [(Exp, Int)]
combine'' (lexp, lval) (rexp, rval) = [(App op lexp rexp, app op lval rval) | op <- ops, valid' op lval rval]
  where ops = [Add, Sub, Mul, Div]

solutions'' :: [Int] -> Int -> [Exp]
solutions'' xs goal = [ans |  choice <- choices xs,
                              (ans, val) <- validexp choice,
                              val == goal]

main :: IO ()
main = print (solutions'' [1,3,7,10,25,50] 765)