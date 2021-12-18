{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char
import System.IO

{-
9.  Take advantage of the nature of the parser to return string it
    didn't consume, and improve the calculator to indicate the vicinity
    where the error occurred, rather than beeping when the parsing fails.

-}

newtype Parser a = P (String -> Maybe (a,String))
parse :: Parser a -> String -> Maybe (a,String)
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
  -- (>>=) :: Monad a -> (a -> Monad b) -> Monad b
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

nat :: Parser Int
nat = do  n <- some (sat isDigit)
          return (read n)

int :: Parser Int
int = do  char '-'
          n <- nat
          return (-n)
      <|>
      nat

space :: Parser ()
space = do  many (sat isSpace)
            return ()

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

tokenStr :: String -> Parser String
tokenStr = token . string

tokenInt :: Parser Int
tokenInt = token int

{-
  expr    ::= expr + term | expr - term | term
  term    ::= term * factor | term / factor | factor
  factor  ::= (expr) | int
  integer ::= ... | -1 | 0 | 1 | ...

  since this grammer contains Left recursion, we transform it as follows

  expr    ::= term opexpr
  opexpr  ::= + term opexpr | - term opexpr | \eps
  term    ::= factor opterm
  opterm  ::= * factor opterm | / factor opterm | \eps
  factor  ::= (expr) | int
  integer ::= ... | -1 | 0 | 1 | ...
-}

data AddSub = Add | Sub
data MulDiv = Mul | Div

expr :: Parser Int
expr = do t <- term
          foldl eval t <$> opexpr
  where
    eval e1 (Add, e2) = e1 + e2
    eval e1 (Sub, e2) = e1 - e2

opexpr :: Parser [(AddSub, Int)]
opexpr =
  do  tokenStr "+"
      t <- term
      oes <- opexpr
      return ((Add,t):oes)
  <|>
  do  tokenStr "-"
      t <- term
      oes <- opexpr
      return ((Sub,t):oes)
  <|>
  return [] -- required to parse only expr and return the rest of the string

term :: Parser Int
term = do f <- factor
          foldl eval f <$> opterm
  where
    eval e1 (Mul, e2) = e1 * e2
    eval e1 (Div, e2) = e1 `div` e2

opterm :: Parser [(MulDiv, Int)]
opterm =
  do  tokenStr "*"
      f <- factor
      ots <- opterm
      return ((Mul, f):ots)
  <|>
  do  tokenStr "/"
      f <- factor
      ots <- opterm
      return ((Div, f):ots)
  <|>
  return []

factor :: Parser Int
factor =
  do  tokenStr "("
      e <- expr
      tokenStr ")"
      return e
  <|>
  tokenInt

------------- test block begin ----------------
test1 = parse expr "1-2-3"
test2 = parse expr "6 / 2 / 2"
test3 = parse expr "2 + (3 * 2 / 4) - 5"
------------- test block end ------------------


box :: [String]
box = [ "+-----------------------+",
        "|                       |",
        "+-----------------------+",
        "|  q  |  c  |  d  |  =  |",
        "+-----------------------+",
        "|  1  |  2  |  3  |  +  |",
        "+-----------------------+",
        "|  4  |  5  |  6  |  -  |",
        "+-----------------------+",
        "|  7  |  8  |  9  |  *  |",
        "+-----------------------+",
        "|  0  |  (  |  )  |  /  |",
        "+-----------------------+" ]

type Pos = (Int, Int)
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

cls :: IO ()
cls = putStr "\ESC[2J"

getCh :: IO Char
getCh = do  hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

beep :: IO ()
beep = putStr "\BEL"

buttons :: String
buttons = standard ++ extra
  where
    standard  = "qcd=123+456-789*0()/"
    extra     = "QCD \ESC\BS\DEL\n"

display :: String -> IO ()
display xs = do writeat (3,2) (replicate 21 ' ')
                writeat (3,2) (reverse (take 21 (reverse xs)))

quit :: IO ()
quit = goto (1,15)

delete :: String -> IO ()
delete [] = standby []
delete xs = standby (init xs)

clear :: IO ()
clear = standby []

indicateErr :: Int -> IO ()
indicateErr id =
  let errbar = "+" ++ replicate (id-2) '-' ++ "V" ++ replicate (25-id-1) '-' ++ "+" in
  writeat (1,1) errbar

handleErr :: String -> String -> IO ()
handleErr xs xs'
  | length xs <= 21 =
    let id = length xs - length xs' + 3 in
    indicateErr id
  | length xs' <= 21 =
    let id = 26 - length xs' - 2 in
    indicateErr id
  | otherwise =
    return ()

eval :: String -> IO ()
eval xs = case parse expr xs of
            Just (n, [])  -> do freshbar
                                standby (show n)
            Just (n, xs') -> do handleErr xs xs'
                                standby xs
            Nothing       -> do beep
                                standby xs
  where
    freshbar = writeat (1,1) (head box)

press :: Char -> String -> IO ()
press c xs = standby (xs ++ [c])

process :: Char -> String -> IO ()
process c xs  | c `elem` "qQ\ESC"     = quit
              | c `elem` "dD\BS\DEL"  = delete xs
              | c `elem` "cC"         = clear
              | c `elem` "=\n"        = eval xs
              | otherwise             = press c xs

warning :: String -> IO ()
warning = writeat (1,14) 

standby :: String -> IO ()
standby xs = do display xs
                c <- getCh
                if c `elem` buttons then
                  process c xs
                else
                  do  beep
                      warning (show c ++ " is not a button.")
                      standby xs

run :: IO ()
run = do  cls
          showbox
          standby []