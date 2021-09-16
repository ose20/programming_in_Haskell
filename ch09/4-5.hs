{-
4.  choices, exps, eval を用いて，[1, 3, 7, 10, 25, 50] に対する可能な式は
    33,665,406個あり，そのうち4,672,540個のみが有効であることを確かめてください．
-}

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Exp = Val Int | App Op Exp Exp

instance Show Exp where
  show (Val n) = show n
  show (App o l r) = aux l ++ show o ++ aux r
    where
      aux (Val n) = show n
      aux e = "(" ++ show e ++ ")"

---------- test begin --------------------------------------------------
e1 = App Mul (App Add (Val 1) (Val 2)) (App Add (Val 3) (Val 4))      --
test1 = show e1                                                       --
---------- test end -----------------------------------------------------


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs
---------- test begin ----------
test2 = subs [1..3]           --
---------- test end ------------


interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
---------- test begin ----------
test3 = interleave 1 [2..4]   --
---------- test end ------------


perms :: [a] -> [[a]]
--perms [] = [[]]
--perms (x:xs) = concatMap (interleave x) (perms xs)
perms = foldr (concatMap . interleave) [[]]
---------- test begin ----------
test4 = perms [1..3]          --
---------- test end ------------


app :: Op -> Int -> Int -> Int
app Add x y = x + y
app Sub x y = x - y
app Mul x y = x * y
app Div x y = x `div` y

type Res = (Exp, Int)

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]
---------- test begin ----------
test5 = split [1..5]          --
---------- test end ------------

results :: [Int] -> [Res]
results [] = []
results [n] = [(Val n, n)]
results xs = [r | (ls, rs) <- split xs,
                  lres <- results ls,
                  rres <- results rs,
                  r <- combine lres rres]

combine :: Res -> Res -> [Res]
combine (lexp, lval) (rexp, rval) =
  [(App op lexp rexp, app op lval rval) | op <- ops, valid op lval rval]
  where ops = [Add, Sub, Mul, Div]
---------- test begin ----------
test6 = results [1..3]        --
---------- test end ------------


choices :: [a] -> [[a]]
choices = concatMap perms . subs

validExpSum :: [Int] -> Int
validExpSum xs = length [e |  choice <- choices xs,
                              (e, _) <- results choice]


results' :: [Int] -> [Exp]
results' [] = []
results' [n] = [Val n]
results' xs = [e |  (ls, rs) <- split xs,
                    lexp <- results' ls,
                    rexp <- results' rs,
                    e <- combine' lexp rexp]

combine' :: Exp -> Exp -> [Exp]
combine' x y = [App op x y | op <- [Add, Sub, Mul, Div]]

expSum :: [Int] -> Int
expSum xs = length [e | choice <- choices xs,
                        e <- results' choice]
ans1 = expSum [1,3,7,10,25,50]
ans2 = validExpSum [1,3,7,10,25,50]


{-
5.  同様に，数値の定義域を整数に拡大すると有効な式の数が 10,839,369 個に増えることを確認してください．
-}

valid' :: Op -> Int -> Int -> Bool
valid' Div x y  = y /= 0 && x `mod` y == 0
valid' _ _ _ = True

results'' :: [Int] -> [Res]
results'' [] = []
results'' [n] = [(Val n, n)]
results'' xs = [(e, v) |  (ls, rs) <- split xs,
                          lres <- results'' ls,
                          rres <- results'' rs,
                          (e, v) <- combine'' lres rres]

combine'' :: Res -> Res -> [Res]
combine'' (lexp, lval) (rexp, rval) =
  [(App op lexp rexp, app op lval rval) | op <- ops, valid' op lval rval]
  where ops = [Add, Sub, Mul, Div]

validExpSum' :: [Int] -> Int
validExpSum' xs = length [e | choice <- choices xs,
                              (e, _) <- results'' choice]

ans3 = validExpSum' [1,3,7,10,25,50]