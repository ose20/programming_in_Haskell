import GHC.Base (Applicative, Alternative)
import Control.Applicative
{-
1.  Haskell の1行コメントを解析するパーサー comment :: Parser () を定義する．
    「--」から「\n」までがコメント
-}

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          Nothing -> Nothing
                          Just (out, inp') -> Just (g out, inp'))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> Just (x, inp))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                          Nothing -> Nothing
                          Just (out, inp') -> parse (fmap out px) inp')

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  mx >>= f = P (\inp -> case parse mx inp of
                          Nothing -> Nothing 
                          Just (out, inp') -> parse (f out) inp')

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a                          
  p <|> q = P (\inp -> case parse p inp of
                        Nothing -> parse q inp
                        Just x -> Just x)

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> Nothing
                    x:xs -> Just (x, xs))

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

comment :: Parser ()
comment = do  string "--"
              many (sat (/= '\n'))
              char '\n'
              return ()

test :: String -> Maybe ((), String)
test = parse comment

--------------- test --------------------
test1 = test 
  "-- this is comment in Haskell \n some program ..."

test2 = test
  "-- これは Haskell の1行コメントです．\n some program ..."

test3 = test
  "some program ..."
