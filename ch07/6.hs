{-
  6.  unfold を使って chop8, map f, iterate f を再定義してください．
-}

unfold' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold' p h t x | p x = []
                | otherwise = h x : unfold' p h t (t x)


-- chop8
chop8 xs = unfold' (== []) (take 8) (drop 8) xs
-- ここ xs を省略すると test2 に型がつかなくなる

test1 = chop8 [1..32]
test2 = chop8 ['a'..'z']


-- map f
map' f = unfold' (== []) (f . head) tail 

test3 = map' succ [1..5]

-- iterate f
iterate' f = unfold' (\_ -> False) id f

test4 = take 10 (iterate' succ 0)

