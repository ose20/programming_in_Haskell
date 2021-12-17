{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char


{-
6.  パーサー expr :: Parser Int を以下のように拡張し，減算と除算を加え，
    自然数の代わりに整数を扱えるようにしてください．

    exp     ::= term ( + exp | - exp | \eps )
    term    ::= factor ( * term | / term | \eps )
    factor  ::= ( exp ) | int
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
  gp <*> xp = P (\inp -> case parse gp inp of
                            Nothing -> Nothing
                            Just (g, inp') -> parse (fmap g xp) inp')

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

nat :: Parser Int
nat = do  xs <- some (sat isDigit)
          return (read xs)

int :: Parser Int
int = do  char '-'
          n <- nat
          return (-n)
      <|> nat

space :: Parser ()
space = do  many  (sat isSpace)
            return ()

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

symbol :: String -> Parser String
symbol s = token (string s)

integer :: Parser Int
integer = token int


expr :: Parser Int
expr = do t <- term
          do  symbol "+"
              e <- expr
              return (t+e)
              <|>
              do  symbol "-"
                  e <- expr
                  return (t-e)
                  <|> return t

term :: Parser Int
term = do f <- factor
          do  symbol "*"
              t <- term
              return (f*t)
            <|>
              do  symbol "/"
                  t <- term
                  return (f `div` t)
                  <|>
                  return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> 
            integer


parseExp = parse expr
test1 = parseExp "1+2*3/4"
test2 = parseExp "1 + (2 * 3) / 4"
test3 = parseExp "-4 * -2 + (5 + 1) / 6 "