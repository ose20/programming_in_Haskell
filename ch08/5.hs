{-
5.  data Exp が宣言されているとして，関数 folde :: (Int -> a) -> (a -> a -> a) -> Exp -> a を定義してください．
    ただし，folde f g は，式の中のそれぞれの Val を f に置換し，Add を g に置換します．
-}

data Exp = Val Int | Add Exp Exp

folde :: (Int -> a) -> (a -> a -> a) -> Exp -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-- test
e1 = Add (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))

test1 = folde id (+) e1
test2 = folde (\x -> [x]) (++) e1