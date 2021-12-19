{-
1.  Where is the redex in the following expression? Also, consider
    whether the redex you selected is the innermost, the outermost,
    both, or none of the above.
-}

{-

1 + (2 * 3)
redex : 2 * 3
kind  : innermost and outermost

(1+2) * (2+3)
redex : (1+2)
kind  : innermost and outermost

redex : (2+3)
kind  : innermost and outermost

fst (1+2, 2+3)
redex : 1+2
kind  : innermost

redex : 2+3
kind  : innermost

redex : fst (1+2, 2+3)
kind  : outermost

(\x -> 1 + x) (2*3)
redex : 2*3
kind  : innermost

redex : (\x -> 1 + x) (2*3)
kind  : outermost

-}