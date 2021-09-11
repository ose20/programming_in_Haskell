{-
  3.  関数 foldr を用いて map f と fileter p を定義してください．
-}

map' :: (a -> b) -> [a] -> [b] 
map' f = foldr (\x seed -> f x : seed) []
test1 = map' succ [1..10]
test2 = map' even [1..4]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x seed -> if p x then x : seed else seed) []
test3 = filter' even [1..10]