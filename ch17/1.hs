import Control.Applicative

{-
1.  Extend the language for mathematical expressions to include exception handling.
    Use the methods described in this chapter to compute a compiler for this language.
-}

data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr
            deriving Show
type Stack = [Maybe Int]

eval :: Expr -> Maybe Int
eval (Val n)      = pure n
eval (Add x y)    = (+) <$> eval x <*> eval y
eval Throw        = empty
eval (Catch x h)  = eval x <|> eval h

data Code = PUSH (Maybe Int) Code | ADD Code | CATCH Code | HALT
            deriving Show

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c     = PUSH (Just n) c
comp' (Add x y) c   = comp' x (comp' y (ADD c))
comp' Throw c       = PUSH Nothing c
comp' (Catch x h) c = comp' x (comp' h (CATCH c))

exec :: Code -> Stack -> Stack
exec (PUSH mi c) s      = exec c (mi : s)      -- mi is named for Maybe Int
exec (ADD c) (n:m:s)    = exec c ( ((+) <$> m <*> n) : s )
exec (CATCH c) (h:x:s)  = exec c ( (x <|> h) : s )
exec HALT s             = s
exec _ _                = [] --- can't occure

e1 = Add Throw (Val 2)
e2 = Add e1 (Val 3)
e3 = Catch e1 (Add (Val 10) (Val 20))

compAndExec e = head (exec (comp e) [])

test1 = compAndExec e1
test2 = compAndExec e2  
test3 = compAndExec e3

{-
    c1 = comp' (Add Throw (Val 2)) HALT
--> comp' Throw (comp' (Val 2) (ADD HALT))
--> PUSH Nothing (comp' (Val 2) (ADD HALT))

    v1 = exec c1 []
--> exec (comp' (Add Throw (Val 2)) HALT) []
--> exec (comp' Throw (comp' (Val 2) (ADD HALT))) []
--> exec (PUSH Nothing (comp' (Val 2) (ADD HALT))) []
--> exec (comp' (Val 2) (ADD HALT)) [Nothing]
--> exec (PUSH (Just 2) (ADD HALT)) [Nothing]
--> exec (ADD HALT) [2, Nothing]
--> exec HALT [(+) <$> Nothing <*> 2]
--> [(+) <$> Nothing <*> 2]

  When evaluating (Add Throw x), I'm wondering if it will return Nothing without evaluating x.

    v2 = exec (comp' (Add Throw (Add (Val 1) (Val 2))) HALT) []
--> exec (comp' Throw (comp' (Add (Val 1) (Val 2)) (ADD HALT))) []
--> exec (PUSH Nothing (comp' (Add (Val 1) (Val 2)) (ADD HALT))) []
--> exec (comp' (Add (Val 1) (Val 2)) (ADD HALT)) [Nothing]
--> exec (comp' (Val 1) (comp' (Val 2) (ADD (ADD HALT)))) [Nothing]
-*> exec (comp' (Val 2) (ADD (ADD HALT))) [1,Nothing]
-*> exec (ADD (ADD HALT)) [1,2,Nothing]
-*> exec (ADD HALT) [3, Nothing]
-*> exec HALT [Nothing]
-*> [Nothing]

This virtual machine is doing useless calculations...
-}

{-
  To Do: drive 4 definitions
    1.  Code(type) and its constructs
    2.  comp(function of type Expr -> Code)
    3.  comp'(function of type Expr -> Code -> Code)
    4.  exec(function of type Code -> Stack -> Stack)
  Specifications :
    1.  exec (comp e) s     = eval e : s
    2.  exec (comp' e c) s  = exec c (eval e : s)
  
  Derivation of comp':
    We want to find some c' of type Code such that
      exec (comp' e c) s = exec c' s
    then we can define [comp' e c = c'].
    --- case: e = Val n
        exec (comp' (Val n) c) s = exec c (eval (Val n) : s)
      = exec c ((Just n) : s)
        (Requirement for c', n, s:
          exec c' s = exec c ((Just n) : s)
          ---> c' = (PUSH n c) is sufficient.
        )
      = exec (PUSH (Just n) c) s
    --- case: e = Add x y
        exec (comp' (Add x y) c) s
      = exec c (eval (Add x y) : s)
      = exec c (( (+) <$> eval x <*> eval y ) : s)
        (Requirement for x, y:
          exec c' s = exec c (( (+) <$> eval x <*> eval y )  : s)
          Generalize:
            exec c' s = exec c ( ((+) <$> eval x <*> eval y) : s)
            we difine [exec (ADD c) (n:m:s) = exec c (( (+) <$> eval x <*> eval y ) :s)]
        )
      = exec (ADD c) (eval y : eval x : s)
      = exec (comp' y (ADD c)) (eval x : s)   --- assumption on y
      = exec (comp' x (comp' y (ADD c))) s    --- assumption on x
    --- case: e = Throw
        exec (comp' Throw c) s
      = exec c (eval Throw : s)
      = exec c (Nothing : s)
        (Requirement:
          exec c (Nothing : s) = exec c' s
          c' = PUSH Nothing c is sufficient.
        )
      = exec (PUSH Nothing c) s
    --- case: e = Catch x h
        exec (comp' (Catch x h) c) s = exec c (eval (Catch x h) : s)
      = exec c ((eval x <|> eval h) : s)
        (Requirement:
          exec c' s = exec c ( (eval x <|> eval h) : s )
            exec c ( (eval x <|> eval h) : s )
          = exec (CATCH c) (eval h : eval x : s)
          We define [c' = CATCH c] then,
            exec (CATCH c) (eval h : eval x : s)
          = exec (comp' h (CATCH c)) (eval x : s)
          = exec (comp' x (comp' h (CATCH c))) s
        )
      = exec (comp' x (comp' h (CATCH c))) s

  Derivation of comp:
    We wan to Find some c of type Code such that
      exec (comp e) s = exec c' s
    then, we can derive the definition [comp e = c'].
    --- case: All (any case)
      exec (comp e) s = eval e : s
      (Requirement:
        eval e : s = exec c' s --- from this c' must include e
        In addition, comp should be defined by using comp'
        Then, we assume [c' = comp' e c'' ]. then,
          eval e : s = exec c' s = exec (comp' e c'') s
        = exec c'' (eval e : s)
        Therefore,
          eval e : s = exec c'' (eval e : s)
        Then, we define [c'' = HALT] and
          exec HALT s = s
      )
    = exec (comp' e HALT) s

  Result:
    data Code = PUSH (Maybe Int) Code | ADD Code | CATCH Code | HALT

    comp :: Expr -> Code
    comp e = comp' e HALT

    comp' :: Expr -> Code -> Code
    comp' (Val n) c     = PUSH (Just n) c
    comp' (Add x y) c   = comp' x (comp' y (ADD c))
    comp' Throw c       = PUSH Nothing c
    comp' (Catch x h) c = comp' x (comp' h (CATCH c))

    exec (PUSH mi c) s      = exec c (mi : s)      -- mi is named for Maybe Int
    exec (ADD c) (n:m:s)    = exec c ( ((+) <$> m <*> n) : s )
    exec (CATCH c) (h:x:s)  = exec c ( (x <|> h) : s )
    exec HALT s             = s
-}