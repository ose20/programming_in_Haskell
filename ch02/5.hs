{-
  空でないリストを受け取り，最後の要素だけを取り除いたリストを返す関数 init を2通り定義せよ
-}

init1 xs = take (length xs - 1) xs

init2 xs = reverse (tail (reverse xs))