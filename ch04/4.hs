{-
  4.  パターンマッチを使って || を4通りの方法で定義してください．
-}

-- 1
True |&| True = True
True |&| False = True
False |&| True = True
False |&| False = False

-- 2
False |&&| False = False
_ |&&| _ = True

-- 3
True |&&&| _ = True
False |&&&| b = b

-- 4
b1 |&&&&| b2  | b1 == b2 = b1
              | otherwise = True

testcases = [(True, True), (True, False), (False, True), (False, False)]
ops = [(|&|), (|&&|), (|&&&|), (|&&&&|)]

do_test fs cases =　
  aux fs
    where
      aux [] = []
      aux (f:fs) = check f : aux fs
        where
          check f = check_all_cases cases
            where
              check_all_cases [] = []
              check_all_cases ((b1, b2):cs) = f b1 b2 : check_all_cases cs

test_res = do_test ops testcases              