{-
  7.  パリティビットを用いて，文字列の2進数への変換器が1ビット誤りを検出できるようにしてください．
-}

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldl (\x y -> 2*x + y) 0

test1 = bin2int [1,1,0,1] -- 13
test2 = bin2int []
test3 = bin2int [0]

unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = reverse . unfold (== 0) (`mod` 2) (`div` 2)

test4 = int2bin 13
test5 = int2bin 0

pad8 :: [Bit] -> [Bit]
pad8 bits = take res (repeat 0) ++ bits
  where res = 8 - length bits

test6 = pad8 [1, 0, 0, 1]
test7 = pad8 []

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = (take 8 xs) : chop8 (drop 8 xs)

test8 = chop8 [1..32]


encode :: String -> [Bit]
encode str = code ++ parity
  where
    parity = if even (count 1 code) then [0] else [1]
    count v = length . filter (== v)
    code = aux str
    aux = concat . map (pad8 . int2bin . ord)

decode :: [Bit] -> String
decode bits = if parity == 1 then error "Error detected by parity bit." else res
  where
    parity = (count 1 bits) `mod` 2
    count v = length . filter (== v)
    res = map (chr . bin2int) (chop8 (rmparity bits))
    rmparity bits = take (length bits - 1) bits

channel :: [Bit] -> [Bit]
channel = id

badchannel :: [Bit] -> [Bit]
badchannel = tail

transmit :: String -> String
transmit = decode . channel . encode
transmit' = decode . badchannel . encode

test9 = transmit "Haskell is interesting!"
test10 = transmit' "I'm a cat"

