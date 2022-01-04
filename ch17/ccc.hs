{-
  memo: G.Hutton and P. Bahr, "Calculating Correct Compilers"
-}

-- 3 Exceptions

data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                  Just n -> case eval y of
                              Just m -> Just (n + m)
                              Nothing -> Nothing
                  Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                    Just n -> Just n
                    Nothing -> eval h

{-
  exec (comp' (Add x y) c) s
=　
  case eval (Add x y) of
    Just n' -> exec c (VAL n' : s)
    Nothing -> fail (Add x y) c s
=
  case eval x of
    Just n -> case eval y of
                Just m -> exec c (VAL (n+m) : s)
                Nothing -> fail (Add x y) c s
    Nothing -> fail (Add x y) c s
= (def exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n+m) : s) )
  case eval x of
    Just n -> case eval y of
                Just m -> exec (ADD c) (VAL m : VAL n : s)
                Nothing -> fail (Add x y) c s
    Nothing -> fail (Add x y) c s
-}