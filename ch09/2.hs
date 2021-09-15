{-
2.  再帰的な関数 isChoice :: Eq a => [a] -> [a] -> Bool を定義してください．
    この関数は，perms や subs を使わずに，一方のリストが他方のリストから選択されたものかを検査します．
-}

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y then ys else y : (remove x ys)
---------- test begin --------------
test1 = remove 1 [1..4]           --
test2 = remove 1 [1,2,3,1,1,4]    --
test3 = remove 10 [1..9]          --
---------- test end ----------------


mem :: Eq a => a -> [a] -> Bool
mem x [] = False
mem x (y:ys) = x == y || mem x ys
---------- test begin ----------
test6 = mem 5 [1..4]          --
test7 = mem 10 [1..1000]      --
---------- test end ------------


isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = mem x ys && isChoice xs ys
---------- test begin --------------
test4 = isChoice [1..4] [1..10]   --
test5 = isChoice [2,3,7] [1..6]   --
---------- test end ----------------
