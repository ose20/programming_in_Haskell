{-
1.  Extend the language for mathematical expressions to include exception handling.
    Use the methods described in this chapter to compute a compiler for this language.
-}

{-
  We give a solution based on the first method of G. Hutton and P. Bahr, "Calculating Correct Compilers".
-}

data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr
            deriving Show
data Elem = VAL Int | HAN Code
            deriving Show
type Stack = [Elem]
data Code = HALT | PUSH Int Code | FAIL | ADD Code | UNMARK Code | MARK Code Code
            deriving Show

eval :: Expr -> Maybe Int
eval (Val n)      = Just n
eval (Add x y)    = case eval x of
                      Just n -> case eval y of
                                  Just m -> Just (n+m)
                                  Nothing -> Nothing
                      Nothing -> Nothing
eval Throw        = Nothing
eval (Catch x h)  = case eval x of
                      Just n -> Just n
                      Nothing -> eval h

comp' :: Expr -> Code -> Code
comp' (Val n) c     = PUSH n c
comp' (Add x y) c   = comp' x (comp' y (ADD c))
comp' Throw c       = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

comp :: Expr -> Code
comp e = comp' e HALT

exec :: Code -> Stack -> Stack
exec HALT s                         = s
exec (PUSH n c) s                   = exec c (VAL n:s)
exec (ADD c) (VAL m : VAL n : s)    = exec c (VAL (n+m) : s)
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)
exec (MARK c' c) s                  = exec c (HAN c' : s)
exec FAIL s                         = fail' s


fail' :: Stack -> Stack
fail' (VAL _ : s) = fail' s
fail' (HAN c : s) = exec c s
fail' []          = []

e1 = Add Throw (Add (Val 10) (Val 20))
e2 = Catch e1 (Add (Val 2) (Val 3))

test1 = exec (comp e1) []
test2 = exec (comp e2) []

{-  Computational behavior

    comp' e1 HALT
  = comp' (Add Throw (Add (Val 10) (Val 20))) HALT
  = comp' Throw (comp' (Add (Val 10) (Val 20)) (Add HALT))
  = FAIL

    exec (comp' e1 HALT) []
  = exec (comp' Throw (comp' (Add (Val 10) (Val 20)) (Add HALT))) []
  = exec FAIL []
  = fails []
  = []


    comp' e2 HALT
  = comp' (Catch e1 (Add (Val 2) (Val 3))) HALT
  = MARK (comp' (Add (Val 2) (Val 3)) HALT) (comp' e1 (UNMARK HALT))

    exec (comp' e2 HALT) []
  = exec (MARK (comp' (Add (Val 2) (Val 3)) HALT) (comp' e1 (UNMARK HALT))) []
  = exec (comp' e1 (UNMARK HALT)) (HAN (comp' (Add (Val 2) (Val 3)) HALT) : [])
  = exec (comp' Throw (comp' (Add (Val 10) (Val 20)) (Add UNMARK HALT))) (HAN (comp' (Add (Val 2) (Val 3)) HALT) : [])
  = exec FAIL (HAN (comp' (Add (Val 2) (Val 3)) HALT) : [])
  = fail' (HAN (comp' (Add (Val 2) (Val 3)) HALT) : [])
  = exec (comp' (Add (Val 2) (Val 3)) HALT) []
  = exec (comp' (Val 2) (comp' (Val 3) (ADD HALT))) []
  = exec (PUSH 2 (comp' (Val 3) (ADD HALT))) []
  = exec (comp' (Val 3) (ADD HALT)) [VAL 2]
  = exec (ADD HALT) [VAL 3, VAL 2]
  = exec HALT [VAL 5]
  = [VAL 5]
-}


{-  Derivation of compiler and virtial machine.
  Specification:
    exec (comp' x c) s = case eval x of
                          Just n -> exec c (VAL n : s)
                          Nothing -> fail s
    
    exec (comp x) s = case eval x of
                        Just n -> VAL n : s
                        Nothing -> fail s

  To Do: derive the definition of comp'
  Approach: find some c' such that
      exec (comp' x c) s = exec c' s
    from which we can then conclude that the definition comp' x c = c' satisfies the
    specification.

    --- case: Val n
        exec (comp' (Val n) c) s = exec c (VAL n : s)
        (define: exec (PUSH n c) s = exec c (VAL n:s))
      = exec (PUSH n c) s
      = exec c' s
    --- case: Throw
        exec (comp' Throw c) s = fail s
        (define: exec FAIL s = fail s)
      = exec FAIL s
      = exec c' s
    --- case: Add x y
        exec (comp' (Add x y) c) s = 
        case eval x of
          Just n -> case eval y of
                      Just m -> exec c (VAL (n+m) : s)
                      Nothing -> fail s
          Nothing -> fail s
        = --- (1)


            (Assumption on y:
              exec (comp' y c) s = case eval y of
                                    Just m -> exec c (VAL m : s)
                                    Nohitng -> fail s
            )

              case eval y of
                Just m -> exec c (VAL (n+m) : s)
                Nothing -> fail s
            (define: exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n+m) : s))
            = case eval y of
                Just m -> exec (ADD c) (VAL m : VAL n : s)
                Nothing -> fail s
            (define: fail (VAL _ : s) = fail s)
            = case eval y of
                Just m -> exec (ADD c) (VAL m : VAL n : s)
                Nothing -> fail (VAL n : s)
            = exec (comp' y (ADD c)) (VAL n : s)
        
        Therefore ...
        (1) = case eval x of
                Just n -> exec (comp' y (ADD c)) (VAL n : s)
                Nothing -> fail s
        = exec (comp' x (comp' y (ADD c))) s
    --- case: Catch x h
        exec (comp' (Catch x h) c) s
      = case eval x of
          Just n -> exec c (VAL n : s)
          Nothing -> case eval h of
                      Just m -> exec c (VAL m : s)
                      Nothing -> fail s
      = case eval x of
          Just n -> exec c (VAL n : s)
          Nothing -> exec (comp' h c) s
          (Requirement:
            fail s = exec (comp' h c) s
          h c are unbound. We assume that the entire handler code comp' h c is provided
          on the stack by means of a HAN constructor with a sigle argument.
            define: fail (HAN c' : s) = exec c' s
          )
      = case eval x of
          Just n -> exec c (VAL n : s)
          Nothing -> fail (HAN (comp' h c) : s)
          (Requirement:
            fail s = fail (HAN (comp' h c) : s)
          but fail (HAN c : s) = ... is already defined. So ...
          New Requirement:
            exec c' (VAL n : HAN (comp' h c) : s) = exec c (VAL n : s)
          define: exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)
          )
      = case eval x of
          Just n -> exec (UNMARK c) (VAL n : HAN (comp' h c) : s)
          Nothing -> fail (HAN (comp' h c) : s)
      = exec (comp' x (UNMARK c)) (HAN (comp' h c) : s)
        (Requirement:
          exec c' s = exec (comp' x (UNMARK c)) (HAN (comp' h c) : s)
          define: exec (MARK c' c) = exec c (HAN c' : s)
        )
      = exec (MARK (comp' h c) (comp' x (UNMARK c))) s

  To Do: derive the definition of comp.
  Approach: find some c' such that
      exec (comp x) s = exec c' s
    from which we can conclude that the definition comp x = c'
    satisfies the specification.
    
    exec (comp e) s
  = case eval e of
      Just n -> VAL n : s
      Nothing -> fail s
    (define : exec HALT s = s)
  = case eval e of
      Just n -> exec HALT (VAL n : s)
      Nothing -> fail s
  = exec (comp' e HALT) s
-}