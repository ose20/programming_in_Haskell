import GHC.Base (Applicative)

data Maybe' a = Nothing' | Just' a
  deriving Show

instance Functor Maybe' where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing' = Nothing' 
    fmap g (Just' x) = Just' (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree 
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node left right) = Node (fmap g left) (fmap g right)

t1 :: Tree String
t1 = Node 
      (Node (Leaf "abc") (Leaf "d"))
      (Node (Leaf "efgh") (Leaf "ij"))
test1 = fmap length t1


{-
instance Functor IO where
  fmap g mx = do { x <- mx; return (g x) }
-}

{-
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y
-}

----------- 12.2.1 ----------------
instance Applicative Maybe' where
  -- pure :: a -> Maybe' a
  pure = Just'
  -- (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  (Just' g) <*> mx = fmap g mx

test2 = pure (+1) <*> Just' 1
test3 = pure (+) <*> Just' 1 <*> Just' 2


{- プレリュードでは以下のように定義されている
instance Applicative [] where
  -- pure :: a -> [a]
  pure a = [a]
  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]
-}
test4 = pure (+1) <*> [1,2,3]
test5 = pure (+) <*> [1,2,3] <*> [10, 20, 30]


{- プレリュードでは以下のように定義されている
instance Applicative IO where
  -- pure :: a -> IO a
  pure = return
  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do {g <- mg; x <- mx; return (g x)}
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

---------- 12.2.2 作用を持つプログラミング ----------
getChars2 :: Int -> IO String
getChars2 n = sequenceA (replicate n getChar)


---------- 12.3 モナド ----------
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- 0 で割ると例外
test6 = eval (Div (Val 1) (Val 0))

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing 
safediv n m = Just (n `div` m)

-- safediv を使って改善
eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case (eval2 x, eval2 y) of
                    (Nothing, _) -> Nothing 
                    (_, Nothing) -> Nothing
                    (Just n, Just m) -> safediv n m

-- 今度は例外にならない
test7 = eval2 (Div (Val 1) (Val 0))

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
            Nothing -> Nothing 
            Just x -> f x

-- これを用いると eval はもっと簡潔になる
eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = eval3 x Main.>>= \n ->
                  eval3 y Main.>>= \m ->
                  safediv n m

-- または次のように書く
eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = do  n <- eval4 x
                      m <- eval4 y
                      safediv n m

-- monad とは
{-
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  return = pure (上書き可能)
-}

----------- 12.3.1 例 ----------
{- 
プレリュードでは次のように定義されている
instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x

リスト型もモナドにできる
instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]
-}

{-
pairs xs ys = xs >>= \x ->
              ys >>= \y ->
              return (x,y)
-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do  x <- xs
                  y <- ys
                  return (x,y)

---------- 12.3.2 State モナド ----------
type State = Int
newtype ST a = S (State -> (a, State))
app :: ST a -> State -> (a, State)
app (S st) = st

-- まずは ST を関手にしよう
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s ->
    let (x, s') = app st s in (g x, s'))

-- その調子で ST を Applicative にもしてみよう
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b 
  stg <*> stx = S (\s ->
    let (g, s') = app stg s in
    let (a, s'') = app stx s' in
    (g a, s''))

-- 最後に ST をモナドにする
instance Monad ST where 
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
    let (xa, s') = app st s in
    let st' = f xa in
    app st' s')

---------- 12.3.3 木構造のラベル付け ----------
{-
                再掲
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

木の葉を，未使用な一意の整数でラベル付けする関数を定義する
-}

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n =
  let (l', n') = rlabel l n in
  let (r', n'') = rlabel r n' in
  (Node l' r', n'')

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
test8 = fst (rlabel tree 0)


-- モナドを使って実装をもっと簡潔にしよう
fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

test9 = fst (app (alabel tree) 0)


---------- 12.3.4 汎用的な関数 ----------
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

-- 次はリストに対する filter :: (a -> bool) -> [a] -> [a] 関数のモナド版を作ろう 
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = do  b <- p x
                        ys <- filterM' p xs
                        return (if b then x:ys else ys)

test10 = filterM' (\x -> [True, False]) [1,2,3]

-- 最後は，プレリュード関数 concat :: [[a]] -> [a] を任意のモナドへ一般化した関数
join' :: Monad m => m (m a) -> m a
join' mmx = do  mx <- mmx
                x <- mx
                return x

-- これは入れ子になった Monad を平坦にする(ネストは2つまでみる)．
test11 = join' [[1,2],[3,4],[5,6]]
test12 = join' [[[1,2],[3,4]], [[5,6],[7,8]]]
test13 = join' (Just (Just 1))
test14 = join' (Just (Just (Just 1)))
test15 = join' (Just (Just Nothing))
test16 = join' (Just Nothing)
test17 = join' Nothing