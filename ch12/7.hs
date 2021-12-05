{-
7.  以下の型 Exp を Functor, Applicative, Monad にする．
    この時，>>= は何を意味しているか
-}

data Exp a = Var a | Val Int | Add (Exp a) (Exp a)
  deriving Show

instance Functor Exp where
  -- fmap :: (a -> b) -> Exp a -> Exp b
  fmap g (Var a) = Var (g a)
  fmap g (Val n) = Val n
  fmap g (Add l r) = Add (fmap g l) (fmap g r)

instance Applicative Exp where
  -- pure :: a -> Exp a
  pure a = Var a
  -- <*> :: Exp (a -> b) -> Exp a -> Exp b
  -- f <*> x の直観的なイメージ
  -- x :: Exp a を辿っていって，Var x が出現したらそれを f に置き換える．ただし，f に存在する Var g は Var (g x) にする
  -- 厳密な証明はまだできないが，この直観的なイメージに基づけば4つのアプリカティブ則を満たしていそうな感じはする
  f <*> (Add l r) = Add (f <*> l) (f <*> r)
  f <*> (Val v) = Val v
  f <*> (Var x) = consTree f x
    where
      consTree (Add l r) x = Add (consTree l x) (consTree r x)
      consTree (Val v) _ = Val v
      consTree (Var f) x = Var (f x) 

instance Monad Exp where
  -- (>>=) :: Exp a -> (a -> Exp b) -> Exp b
  -- mx >>= f の直観的なイメージ
  -- mx を走査していって，Var x が出現したらそれを f x で置き換える
  (Add l r) >>= f = Add (l >>= f) (r >>= f)
  (Val v) >>= _ = Val v
  (Var x) >>= f = f x