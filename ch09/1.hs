{-
1.  関数合成 concat 及び map の代わりにリスト内包表記を使って組み合わせの関数 choices を再定義してください．
-}


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ (map (x:) yss)
  where yss = subs xs
---------- test begin ----------
test1 = subs [1..3]           --
---------- test end ------------


interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) 
---------- test begin ----------
test2 = interleave 1 [2..4]   --
---------- test end ------------


perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concat (map (interleave x) (perm xs))
---------- test begin ----------
test3 = perm [1..3]           --
---------- test end ------------


choices :: [a] -> [[a]]
choices xs = [choice | sub <- subs xs, choice <- perm sub]
---------- test begin ----------
test4 = choices [1..3]        --
test5 = choices [1..4]        --
---------- test end ------------
