{-
  7.  [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]] を1つの生成器を持つリスト内包表記2つでも表現できることを示してください．
      ヒント: concat を使おう
-}

test = [[(x, y) | y <- [4..6]] | x <- [1..3]]