import Data.Char
import Data.List

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- 7.3 畳み込み関数 foldr
fold_right :: (a -> b -> b) -> b -> [a] -> b
fold_right f v []     = v
fold_right f v (x:xs) = f x (fold_right f v xs)

sum' = fold_right (+) 0
length' = fold_right (\x -> \y -> y + 1) 0
reverse' = fold_right (\x xs -> xs ++ [x]) []

-- 7.4 畳み込み関数 foldl
fold_left :: (a -> b -> a) -> a -> [b] -> a
fold_left f v []      = v
fold_left f v (x:xs)  = fold_left f (f v x) xs

-- 7.6 文字列の二進数変換器
type Bit = Int
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weigths bits]
  where
    weigths = iterate (*2) 1

bin2int' = fold_right (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []  = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.7 投票アルゴリズム
-- 7.7.1 比較多数得票

votes :: [String]
votes = ["red", "blue", "green", "blue", "blue", "red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- 7.7.2 別の投票アルゴリズム

ballots :: [[String]]
ballots = [ ["red", "green"],
            ["blue"],
            ["green", "red", "blue"],
            ["blue", "green", "red"],
            ["green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
              [c]     -> c
              (c:cs)  -> winner' (elim c bs)