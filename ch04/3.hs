{-
  3.  空リストに対しては空リストを返すような tail 関数 safetale を次の3つの方法で定義してください．
-}

-- 条件式
safetail1 xs = if null xs then [] else tail xs

test1 = safetail1 [1..10]
test2 = safetail1 []

-- ガード付きの等式
safetail2 xs  | null xs = []
              | otherwise = tail xs

test3 = safetail2 [1..10]
test4 = safetail2 []

-- パターンマッチ
safetail3 [] = []
safetail3 (x : rest) = rest

test5 = safetail3 [1..10]
test6 = safetail3 []
