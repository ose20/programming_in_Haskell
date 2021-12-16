---------- 13.2 関数としてのパーサー ----------

-- type Parser a = String -> [(a, String)]
-- ST a の一般化として考えられる


---------- 13.3 基礎的な定義 ----------

import Control.Applicative
import Data.Char
import GHC.Base (Alternative)
import System.IO


newtype Parser a = P (String -> [(a, String)])
parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

-- item という基礎的なパーサーを定義する．
-- 入力文字列が空なら失敗し，それ以外は最初の文字を消費する
item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

test1 = parse item ""
test2 = parse item "abc"

---------- 13.4 パーサーの連接 ----------
-- パーサーを Functor, Applicative, Monad のインスタンスにする．
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          [] -> []
                          [(v,out)] -> [(g v, out)]
                          _ -> [])

test3 = parse (fmap toUpper item) "abc"
test4 = parse (fmap toUpper item) ""

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp ->
    case parse pg inp of
      [] -> []
      [(g, inp')] -> parse (fmap g px) inp'
      _ -> [])

test5 = parse (pure 1) "abc"
-- 3文字消費し，2つ目の文字を捨てて，1つ目と3つ目の文字を返すパーサーをアプリカティブスタイルで書く
three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where g x y z = (x, z)

test6 = parse three "abcdef"
-- 入力文字列が足りなかったら自動的に失敗になる．えらい！
test7 = parse three "ab"

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of 
                        [] -> []
                        [(x, out)] -> parse (f x) out
                        _ -> [])

three2 :: Parser (Char, Char)
three2 = do x <- item
            item
            z <- item
            return (x,z)

{-
  three2 =  item >>= \x ->
            item >>= \_ ->
            item >>= \z ->
            return (x,z)
-}

--------------- 13.5 選択 ---------------

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

直観的には，empty は失敗する選択肢を表し，<|> は選択を表す．
以下の則を満たしている必要がある
empty <|> x = x
x <|> empty = x
x <|> (y <|> z) = (x <|> y) <|> z

instance Alternative Maybe where
  -- empty :: Maybe a
  empty = Nothing

  -- (<|>) :: Maybe a -> Maybe a -> Maybe a
  Nothing <|> my = my
  (Just x) <|> _ = Just x
-}

{-
Parser を Alternative のインスタンスにする
empty は入力文字列に関係なく常に失敗するパーサー．
<|> はパーサーを選択する．ここでは最初のパーサーが成功するならその結果を，失敗するなら同じ文字列に2つ目のパーサーを適用した結果を返す．
-}

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser
  p <|> q = P (\inp -> case parse p inp of
                        [] -> parse q inp
                        [(v,out)] -> [(v,out)]
                        _ -> [])

test8 = parse empty "abc"
test9 = parse (item <|> return 'd') "abc"
test10 = parse (empty <|> return 'd') "abc"


--------------- 13.6 派生関数 ---------------
{-
述語 p を満たす一文字用のパーサー
-}

sat :: (Char -> Bool) -> Parser Char
sat p = do  x <- item
            if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

test11 = parse digit "1bc"

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do  char x
                    string xs
                    return (x:xs)
{-
string (x:xs) = do  (char x) >>= \_ ->
                    (string xs) >>= \_ ->
                    return (x:xs)
-}                    

test12 = parse (string "abc") "abcdef"
test13 = parse (string "abc") "ab1234"


ident :: Parser String
ident = do  x <- lower
            xs <- many alphanum
            return (x:xs)

nat :: Parser Int 
nat = do  xs <- some digit
          return (read xs)

space :: Parser ()
space = do  many (sat isSpace)
            return ()

test14 = parse ident "abc def"
test15 = parse nat "123 abc"
test16 = parse space "   abc"            

{-
nat を用いて整数のパーサーを定義
-}

int :: Parser Int
int = do  char '-'
          n <- nat
          return (-n)
      <|> nat

test17 = parse int "-123 abc"

---------------- 13.7 空白の扱い ---------------
token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

{-
前後の空白を無視できる識別子，自然数，特定文字列のパーサー
-}              
identifier :: Parser String
identifier = token ident

natural :: Parser Int 
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

{-
自然数のリストを解析するパーサー
-}
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do  symbol ","
                          natural)
          symbol "]"
          return (n:ns)

test18 = parse nats " [1,  2,3] abc"          


--------------- 13.8 数式 ---------------
expr :: Parser Int
expr = do t <- term
          do  symbol "+"
              e <- expr
              return (t + e)
            <|> return t

term :: Parser Int
term = do f <- factor
          do  symbol "*"
              t <- term
              return (f * t)
            <|> return f

factor :: Parser Int
factor =  do  symbol "("
              e <- expr
              symbol ")"
              return e
            <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"
            _ -> error "can't occur"

test19 = eval "2*3+4"
test20 = eval "2*(3+4)"
test21 = eval "2*3^4"
test22 = eval "one plus two"


--------------- 13.9 計算機 ---------------
box :: [String]
box = [ "+---------------+",
        "|               |",
        "+---------------+",
        "| q | c | d | = |",
        "+---------------+",
        "| 1 | 2 | 3 | + |",
        "+---------------+",
        "| 4 | 5 | 6 | - |",
        "+---------------+",
        "| 7 | 8 | 9 | * |",
        "+---------------+",
        "| 0 | ( | ) | / |",
        "+---------------+" ]

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)
writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")                  



getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]    

display :: String -> IO ()
display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do  display xs
              c <- getCh
              if elem c buttons then
                process c xs
              else
                do  beep
                    calc xs

beep :: IO ()
beep = putStr "\BEL"                    

process :: Char -> String -> IO ()
process c xs  | elem c "qQ\ESC"     = quit
              | elem c "dD\BS\DEL"  = delete xs
              | elem c "=\n"        = eval' xs
              | elem c "cC"         = clear
              | otherwise           = press c xs                    

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr xs of
            [(n,[])]  -> calc (show n)
            _         -> do beep
                            calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do  cls
          showbox
          clear


