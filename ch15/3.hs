{-
3.  For the definition mult = \x -> \y -> x * y, show that the
    evaluation of the expression mult 3 4 is a four-step procedure.
-}

{-

mult 3 4
---> (\x -> \y -> x * y) 3 4
---> (\y -> 3 * y) 4
---> 3 * 4
---> 12

-}