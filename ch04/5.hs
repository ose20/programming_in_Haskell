{-
  5, 6.  他のプレリュード関数や演算子を使わずに，論理積 && を実現してください．
-}

and' b1 b2 = 
  if b1 == True 
  then 
    if b2 == True then True else False 
  else False

and'' b1 b2 = if b1 == True then b2 else False

cs = [(True, True), (True, False), (False, True), (False, True)]
ops = [and', and'']

test_res = do_test cs ops
  where
    do_test cs ops = aux ops
      where
        aux [] = []
        aux (f:fs) = f_test f cs : aux fs
          where
            f_test f [] = []
            f_test f ((b1, b2) : cs) = f b1 b2 : f_test f cs