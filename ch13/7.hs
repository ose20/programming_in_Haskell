{-# LANGUAGE LambdaCase #-}
import Control.Applicative 
import Data.Char
{-
7.  構文規則をさらに拡張して冪乗を扱えるようにする
    冪乗は右結合で，乗算や除算より高い結合順位を持つが，括弧や数値よりは低い優先順位をもつ
-}

{-
    文法は以下の通り．
    expr    ::= term1 ( + expr | - expr | \eps )
    term1   ::= term2 ( * term | / term | \eps )
    term2   ::= factor ( ^ term2 | \eps )
    factor  ::= (expr) | int
    int     ::= ... | -1 | 0 | 1 | ...
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
  -- (>>=) :: Parser a -> (a -> Parser b) -> (Parser b)
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
            x:xs -> Just (x,xs) )

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
      <|> nat

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

tokenStr :: String -> Parser String
tokenStr s = token (string s)

tokenInt :: Parser Int
tokenInt = token int

exprp :: Parser Int
exprp =
  do  t <- term1p
      do  tokenStr "+"
          e <- exprp
          return (t+e)
        <|>
        do  tokenStr "-"
            e <- exprp
            return (t-e)
          <|>
          return t

term1p :: Parser Int
term1p =
  do  t1 <- term2p
      do  tokenStr "*"
          t2 <- term1p
          return (t1*t2)
        <|>
        do  tokenStr "/"
            t2 <- term1p
            return (t1 `div` t2)
          <|>
          return t1

term2p :: Parser Int
term2p =
  do  f <- factorp
      do  tokenStr "^"
          t <- term2p
          return (f^t)
        <|>
        return f

factorp :: Parser Int
factorp = 
  do  tokenStr "("
      e <- exprp
      tokenStr ")"
      return e
    <|>
    intp

intp :: Parser Int
intp = tokenInt

test :: String -> Maybe (Int, String)
test = parse exprp

test1 = test "2^2^3"
test2 = test "2 ^ 3 * 4"