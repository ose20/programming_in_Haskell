{-
  1.  [f x | x <- xs, p x] を，高階関数 map と filter を使って書き直してください．
-}

filtapp f p  = map f . filter p

test1 = filtapp succ even [1..10]
test2 = filtapp (++ " lol") (\_ -> True) ["fjeo", "jfoe", "qeio"]