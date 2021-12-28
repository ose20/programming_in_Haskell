{-
9.  Show that the Maybe type satisfies Applicative rules.

    note.
      instance Functor Maybe where
        -- fmap :: (a -> b) -> Maybe a -> Maybe b
        fmap _ Nothing  = Nothing
        fmap g (Just x) = Just (g x)
      instance Applicative Maybe where
        -- pure :: x -> Maybe x
        pure x = Just x
        -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
        Nothing <*> _     = Nothing
        _ <*> Nothing     = Nothing
        Just f <*> Just x = Just (f x)

      Applicative rules
        pure id <*> x     = x
        pure (g x)        = pure g <*> pure x
        x <*> pure y      = pure (\g -> g y) <*> x
        x <*> (y <*> z)   = (pure (.) <*> x <*> y) <*> z
      
    Proof of [pure id <*> x = x]
      --- case: Nothing
      pure id <*> Nothing = Nothing (straightforward)
      --- case: Just x
      pure id <*> (Just x) = (Just id) <*> (Just x) = Just (id x) = Just x
    QED:

    Proof of [pure (g x) = pure g <*> pure x]
      pure (g x) = Just (g x)
      pure g <*> pure x = Just g <*> Just x = Just (g x)
    QED:

    Proof of [x <*> pure y = pure (\g -> g y) <*> x]
      --- case: x = Nothing
      x <*> pure y = Nothing
      pure (\g -> g y) <*> x = Nothing
      --- case: x = Just x'
      Just x' <*> pure y = Just x' <*> Just y = Just (x' y)
      pure (\g -> g y) <*> (Just x') = Just (\g -> g y) <*> (Just x') = Just (x' y)
    QED:

    Proof of [x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z]
      --- case: At least one of x, y, or z is Nothing
      Both sides are Nothing (straightforward)
      --- case: x = Just x', y = Just y', z = Just z'
      x <*> (y <*> z) = Just x' <*> (Just y' <*> Just z')
      = Just x' <*> Just (y' z') = Just (x' (y' z'))
      (pure (.) <*> x <*> y) <*> z = (pure (.) <*> Just x' <*> Just y') <*> Just z'
      = Just (\z -> x' (y' z)) <*> Just z'
      = Just (x' (y' z'))
    QED:
-}