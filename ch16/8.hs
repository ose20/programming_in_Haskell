{-
8.  Given the following type definition and instance declaration,
    show that the Tree type satisfies the Funtor rule.

      data Tree a = Leaf a | Node (Tree a) (Tree a)

      instance Functor Tree where
        -- fmap :: (a -> b) -> Tree a -> Tree a
        fmap g (Leaf a) = Leaf (g x)
        fmap g (Node l r) = Node (fmap g l) (fmap g r)

    Proof of fmap id = id
    --- case: Leaf x
      fmap id (Leaf x) = Leaf (id x) = Leaf x = id (Leaf x)
    --- case: Node l r
      fmap id (Node l r) = Node (fmap id l) (fmap id r) = Node (id l) (id r) = Node l r = id (Node l r)
    QED:

    Proof of fmap (g . h) = fmap g . fmap h
    --- case: Leaf x
    fmap (g . h) (Leaf x) = Leaf (g (h x))
    (fmap g . fmap h) (Leaf x) = fmap g (fmap h (Leaf x)) = fmap g (Leaf (h x))
    = Leaf (g (h x))
    --- case: Node l r
    fmap (g . h) (Node l r) = Node (fmap (g . h) l) (fmap (g . h) r) = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
    (fmap g . fmap h) (Node l r) = fmap g (Node (fmap h l) (fmap h r))
    = Node (fmap g (fmap h l)) (fmap g (fmap h r)) = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
    QED:


-}