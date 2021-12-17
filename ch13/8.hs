{-# LANGUAGE LambdaCase #-}

import Control.Applicative 
import Data.Char
{-
8.  自然数と左結合の減算演算子を使った数式について考える．
    a.  構文規則へ変換する
    b.  それを exprp :: Parser Int に変換する
    c.  このパーサーの問題点は何か
    d.  問題の解決策を示す
-}


newtype Parser a = P (String -> Maybe (a, String))
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of 
                          Nothing -> Nothing
                          Just (x, inp') -> Just (g x, inp'))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> Just (x, inp))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                          Nothing -> Nothing
                          Just (g, inp') -> parse (fmap g px) inp')

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  mx >>= f = P (\inp -> case parse mx inp of 
                          Nothing -> Nothing
                          Just (x, inp') -> parse (f x) inp')

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                        Nothing -> parse q inp
                        Just res -> Just res)


item :: Parser Char
item = P (\case
            [] -> Nothing
            x:xs -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat p = do  x <- item
            if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do  char x
                    string xs
                    return (x:xs)

space :: Parser ()
space = do  many (sat isSpace)
            return ()

nat :: Parser Int
nat = do  n <- some (sat isDigit)
          return (read n)

int :: Parser Int
int = do  char '-'
          n <- nat
          return (-n)
        <|>
        nat

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

tokenStr :: String -> Parser String
tokenStr s = token (string s)

tokenInt :: Parser Int
tokenInt = token int

{-

a.
  expr    ::= expr - factor | expr + factor | factor
  factor  ::= (expr) | int
  int     ::= ... | -1 | 0 | 1 | ...

b.
exprp :: Parser Int
exprp =
  do  e <- exprp
      do  tokenStr "-"
          f <- factorp
          return (e-f)
  <|>
    factorp  


factorp :: Parser Int
factorp =
  do  tokenStr "("
      e <- exprp
      tokenStr ")"
      return e
    <|>
    tokenInt

c.
規則が左再帰になっていて，例えば exprp が止まらない

d.
次のように直す（『最新コンパイラ構成技法』p47を参照した）

expr    ::= term opexpr
opexpr  ::= + term opexpr | - term opexpr | \eps
term    ::= (epxr) | int
int     ::= ... | -1 | 0 | 1 | ...
-}

data Op = Add | Sub

expr :: Parser Int
expr =
  do  t <- term
      do  e <- opexpr
          return (t+e)
        <|>
        return t

opexpr :: Parser Int
opexpr =
  do  opterms <- some opterm
      return (foldl (\e (op, n) -> eval op e n) 0 opterms)
  where
    eval Add e n = e + n
    eval Sub e n = e - n

opterm :: Parser (Op, Int)
opterm =
  do  tokenStr "+"
      t <- term
      return (Add, t)
    <|>
    do  tokenStr "-"
        t <- term
        return (Sub, t)

term :: Parser Int
term =
  do  tokenStr "("
      e <- expr
      tokenStr ")"
      return e
  <|>
  tokenInt


test :: String -> Maybe (Int, String)
test = parse expr

test1 = test "1+2-3"
test2 = test "1+2-(1+2)+(2-(1-1)+4)-2-2"
test3 = test "12"