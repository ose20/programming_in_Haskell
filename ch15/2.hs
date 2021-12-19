{-
2.  Explain why outermost reduction is more appropriate than innermost reduction
    when evaluating the expression fst (1+2, 2+3)
-}

{-

outermost reduction can reduce the number of evaluations.

- outermost reduction
fst (1+2, 2+3)
---> 1+2
---> 3

2-step

- innermost reduction
fst (1+2, 2+3)
---> fst (3, 2+3)
---> fst (3, 5)
---> 3

3-step

-}