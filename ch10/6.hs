{-
6.  getCh を用いて，アクション readLine :: IO String を定義してください．
-}

mytail :: [a] -> [a]
mytail [] = []
mytail (x:xs) = xs

iter :: String -> IO String
iter res = do
  x <- getChar
  if x == '\n' then
    return (reverse res)
  else if x == '\b' || x == '\DEL' then
    iter (mytail res)
  else
    iter (x:res)

readLine :: IO String
readLine = iter ""
