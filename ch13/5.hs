import Control.Applicative
import GHC.Base (Alternative)
import Data.Char
{-
5.  数式の型 Expr を適切に定義して，数式のパーサーが型 expr :: Parser Expr を持つように改造してください．
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
  pf <*> px = P (\inp -> case parse pf inp of
                            Nothing -> Nothing 
                            Just (f, inp') -> parse (fmap f px) inp')

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
                          Just x -> Just x)

item :: Parser Char
item = P (\inp -> case inp of
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

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do  xs <- some digit
          return (read xs)

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

symbol :: String -> Parser String
symbol s = token (string s)

natural :: Parser Int
natural = token nat

data Expr = ExpAdd Term Expr | ExpTerm Term
data Term = TermMul Factor Term | TermFactor Factor
data Factor = FactorParen Expr | FactorNat Nat
newtype Nat = NatVal Int

instance Show Expr where
  show = exprToString 0
instance Show Term where
  show = termToString 0
instance Show Factor where
  show = factorToString 0
instance Show Nat where
  show = natToString 0

exprToString :: Int -> Expr -> String
exprToString indent (ExpAdd term expr) =
  replicate indent '\t' ++ "ExpAdd\n" ++
  termToString (indent+1) term ++
  exprToString (indent+1) expr
exprToString idt (ExpTerm term) =
  replicate idt '\t' ++ "ExpTerm\n" ++
  termToString idt term

termToString :: Int -> Term -> String
termToString idt (TermMul factor term) =
  replicate idt '\t' ++ "TermMul\n" ++
  factorToString (idt+1) factor ++
  termToString (idt+1) term
termToString idt (TermFactor factor) =
  replicate idt '\t' ++ "TermFactor\n" ++
  factorToString idt factor

factorToString :: Int -> Factor -> String
factorToString idt (FactorParen expr) =
  replicate idt '\t' ++ "FactorParen\n" ++
  exprToString idt expr
factorToString idt (FactorNat nat) = 
  replicate idt '\t' ++  "FactorNat\n" ++
  natToString idt nat

natToString :: Int -> Nat -> String
natToString indent (NatVal n) = replicate indent '\t' ++ show n ++ "\n\n"


expr :: Parser Expr
expr =  do  t <- term
            do  symbol "+"
                e <- expr
                return (ExpAdd t e)
              <|> return (ExpTerm t)

term :: Parser Term
term = do f <- factor
          do  symbol "*"
              t <- term
              return (TermMul f t)
            <|> return (TermFactor f)

factor :: Parser Factor
factor =  do  symbol "("
              e <- expr
              symbol ")"
              return (FactorParen e)
            <|> 
          do  n <- natp
              return (FactorNat n)

natp :: Parser Nat
natp = do n <- natural
          return (NatVal n)


parseExpr :: String -> Maybe (Expr, String)
parseExpr = parse expr

test2 = parseExpr "2+3+4"
test3 = parseExpr "1+2*(3+4)"