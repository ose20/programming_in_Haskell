{-
  8.  関数 positions を関数 find を使って再定義してください．
-}

find k t = [v | (k', v) <- t, k == k']

positions elt xs =
  find elt (zip xs [0..])

test1 = positions False [True, False, False, True, True, False]