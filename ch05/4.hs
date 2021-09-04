{-
  4. ある要素 x が n 個からなるリストを生成する関数 replicate を定義してください．
-}

replicate' n x = [x | _ <- [1..n]]