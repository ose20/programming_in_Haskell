{-
6.  folde を使って，Exp を対応する Int に変換する関数 eval を定義してください．
    また，Exp のなかに整数がいくつあるか数える関数 size を定義してください．
-}

data Exp = Val Int | Add Exp Exp

folde :: (Int -> a) -> (a -> a -> a) -> Exp -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Exp -> Int
eval = folde id (+)

e1 = Add (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))
test1 = eval e1


size :: Exp -> Int
size = folde (\_ -> 1) (+)

test2 = size e1