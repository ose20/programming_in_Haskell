{-
7.  Show that the Maybe type satisfies the Functor rule.

    note:
      instance Functor Maybe where
        -- fmap :: (a -> b) -> Maybe a -> Maybe b
        fmap g Nothing    = Nothing
        fmap g (Just x)  = Just (f x)
      
      Functor rule
        fmap id     = id
        fmap (g. h) = fmap g. fmap h

    Proof of rule: fmap id = id
    -- case: Nothing
    fmap id Nothing = Nothing = id Nothing
    -- case: (Just x)
    fmap id (Just x) = Just (id x) = Just x = id (Just x)

    Proof of rule: fmap (g . h) = fmap g . fmap h
    -- case: Nothing
    fmap (g . h) Nothing = Nothing
    (fmap g . fmap h) Nothing = Nothing
    -- case: (Just x)
    fmap (g . h) (Just x) = Just ((g. h) x) = Just (g (h x))
    (fmap g . fmap h) (Just x) = fmap g (fmap h (Just x)) = fmap g (Just h x) = Just (g (h x))

-}