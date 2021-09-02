signum' :: Int -> Int
signum' n = if n < 0 then -1 else
              if n == 0 then 0 else 1

abs n | n >= 0 = n
      | otherwise = -n              

signum'' n  | n < 0 = -1
            | n == 0 = 0
            | otherwise = 1 

-- (&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

first (x, _) = x

-- test
test ['a', _, _] = True
test _ = False

head' (x : _) = x

odds n = map (\x -> 2 * x + 1) [0..n-1]