fac 0 = 1
fac n = n * fac (n-1)

test1 = fac 5

fac' n  | n == 0 = 1
        | otherwise = n * fac' (n-1)

test2 = fac' 5

insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : (insert x ys)

isort [] = []
isort (x : xs) = insert x (isort xs)

test3 = isort [1, 3, 4938, 34, 32, 9, 22]
test4 = isort [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

-- 6.3

myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : zip xs ys

test5 = zip [1..] "abcdefghijklmnopqrstuvwxyz"

teardrop 0 xs = xs
teardrop _ [] = []
teardrop n (x:xs) = teardrop (n-1) xs

test6 = teardrop 3 [1..6]

-- 6.4 多重再帰

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

test7 = qsort [3,2,67,1,0]

-- 6.5 相互再帰

myeven 0 = True
myeven n = myodd (n-1)

myodd 0 = False
myodd n = myeven (n-1)

evens [] = []
evens (x:xs) = x : odds xs

odds [] = []
odds (_:xs) = evens xs

test8 = evens "abcdefghijklmn"
test9 = odds "abcdefghijklmn"