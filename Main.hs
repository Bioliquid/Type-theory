{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Arrow (first)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
	
verify :: Maybe (Expr, String) -> String
verify (Just (l, r)) = show l
verify Nothing = "Invalid expression"
	
instance Functor Parser where
	fmap f (Parser g) = Parser $ fmap (first f) . g
 
instance Applicative Parser where
	pure a = Parser $ Just . (a, )
	Parser f <*> Parser x = Parser $ \s1 -> do
		(fv, s2) <- f s1
		(xv, s3) <- x s2
		return (fv xv, s3)

instance Alternative Parser where
	Parser a <|> Parser b = Parser $ \c -> a c <|> b c
		
option v p = p <|> pure v

satisfy p = Parser impl where
	impl (c : cs) = if p c then Just (c, cs) else Nothing
	impl _ = Nothing
 
char c = satisfy (== c)
string s = traverse char s
lower = satisfy isLower
digit = satisfy isDigit
space = satisfy isSpace

many1 p = (:) <$> p <*> many p

type Name = String
data Expr = App Expr Expr | Abs Name Expr | Var Name deriving (Eq)
instance Show Expr where
	show (App a b) = concat ["(", show a, " ", show b, ")"]
	show (Abs n e) = concat ["(\\", n, ".", show e, ")"]
	show (Var n) = n

parser = sp *> expr
expr = flip ($) <$> app <*> option id (flip App <$> abst) <|> abst
abst = Abs <$> (tok "\\" *> ident) <*> (tok "." *> expr)
app = foldl1 App <$> many1 atom
atom = braced <|> var
braced = tok "(" *> expr <* tok ")"
var = Var <$> ident
ident = (:) <$> lower <*> many (lower <|> digit) <* sp
tok s = string s *> sp
sp = many space

main = interact $ verify . parse parser
