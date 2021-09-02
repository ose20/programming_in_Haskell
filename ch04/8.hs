{-
  8.  luhn アルゴリズムの定義
-}

luhnDouble x =
  if res > 9 then res - 9 else res
    where
      res = 2 * x

-- fold_left f seed [] = seed
-- fold_left f seed (x:xs) = fold_left (f seed x) xs

-- fold_left :: (a -> b -> a) -> a -> [b] -> a
-- fold_left f seed xs | null xs = seed
--                    | otherwise = fold_left (f seed (head xs)) (tail xs)


-- え，上がダメになる．なぜ？

luhn x y z w =
  if sum `mod` 10 == 0 then True else False
    where 
      sum = luhnDouble x + y + luhnDouble z + w

test1 = luhn 1 7 8 4 -- True
test2 = luhn 4 7 8 3 -- False
