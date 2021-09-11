{-
8.  恒等式の検査器を拡張して，命題の中で論理和と同値を扱えるようにしてください．
-}

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Eq Prop Prop

type Assoc k v = [(k, v)]
type Env = Assoc Char Bool

find :: Env -> Char -> Bool
find e k = head [v' | (k', v') <- e, k == k']

eval :: Prop -> Env -> Bool
eval (Const b) e = b
eval (Var v) e = find e v
eval (Not p) e = not (eval p e)
eval (And p q) e = (eval p e) && (eval q e)
eval (Or p q) e = (eval p e) || (eval q e)
eval (Imply p q) e = (eval p e) <= (eval q e)
eval (Eq p q) e = (eval p e) == (eval q e)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups (filter (/= x) xs))

getVars :: Prop -> [Char]
getVars p = rmdups (aux p)
  where
    aux :: Prop -> [Char]
    aux (Const b) = []
    aux (Var v) = [v]
    aux (Not p) = aux p
    aux (And p q) = aux p ++ aux q
    aux (Or p q) = aux p ++ aux q
    aux (Imply p q) = aux p ++ aux q
    aux (Eq p q) = aux p ++ aux q 

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

makeEnvs :: Prop -> [Env]
makeEnvs p = map (zip vars) (bools n)
  where
    vars = getVars p
    n = length vars

isTaut :: Prop -> Bool
isTaut p = and [eval p e | e <- makeEnvs p]


-- test
p1 = Eq (Var 'A') (Var 'A')
p2 = Eq (Var 'A') (Imply (Var 'A') (Var 'B'))
p3 = Or (Var 'A') (Not (Var 'A'))
p4 = Eq
      (Imply (Var 'A') (Var 'B'))
      (Or (Not (Var 'A')) (Var 'B'))

test1 = isTaut p1
test2 = isTaut p2
test3 = isTaut p3
test4 = isTaut p4