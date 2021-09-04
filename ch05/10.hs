{-
  10. シーザー暗号のプログラムを変更して大文字も扱えるようにしてください．
-}

import Data.Char

lower2int c = ord c - ord 'a'
int2lower n = chr (ord 'a' + n)

upper2int c = ord c - ord 'A'
int2upper n = chr (ord 'A' + n)

shift n c | isLower c = int2lower ((lower2int c + n) `mod` 26)
          | isUpper c = int2upper ((upper2int c + n) `mod` 26)
          | otherwise = c

encode n xs = [shift n x | x <- xs]

table = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

isAlphabet c = isLower c || isUpper c
count xs f = sum [1 | x <- xs, f x]
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs xs = [percent (count xs (\y -> y == x || y == chr (ord x + 32))) m | x <- ['A'..'Z']]
  where m = count xs isAlphabet

chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
rotate n xs = drop n xs ++ take n xs -- 要素を n 個''左に''ずらす．

positions x xs = [i | (i, v) <- zip [1..] xs, x == v]

crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs