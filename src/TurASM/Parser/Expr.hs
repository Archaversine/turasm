module TurASM.Parser.Expr (parseExpr, varString) where

import Data.Functor

import Prelude hiding (div, mod)

import TurASM.Parser.Types
import TurASM.Parser.Getter

import Text.Megaparsec
import Text.Megaparsec.Char

parseExpr :: Parser Expr 
parseExpr = try (binOp Add '+')
        <|> try (binOp Sub '-') 
        <|> try (binOp Mul '*') 
        <|> try (binOp Div '/') 
        <|> try (binOp Mod '%') 
        <|> char '(' *> parseExpr <* char ')'
        <|> number 
        <|> luaString
        <|> luaList
        <|> luaBool
        <|> var 
        <|> getter 
        <|> emptyTable

binOp :: (Expr -> Expr -> Expr) -> Char -> Parser Expr 
binOp e c = do 
    e1 <- char '(' *> hspace *> parseExpr <* hspace
    e2 <- char c *> hspace *> parseExpr <* hspace <* char ')'

    return (e e1 e2)

number :: Parser Expr 
number = do 
    lhs <- some digitChar 
    rhs <- optional $ char '.' *> some digitChar

    let rest = case rhs of 
            Nothing -> "" 
            Just x  -> '.' : x

    return $ Number $ lhs <> rest

luaString :: Parser Expr 
luaString = do 
    s <- char '"' *> manyTill anySingle (char '"')

    return $ LuaString s

luaList :: Parser Expr
luaList = do 
    char '[' *> hspace
    es <- parseExpr `sepBy` (hspace *> char ',' <* hspace)
    void $ char ']'

    return (LuaList es)

luaBool :: Parser Expr 
luaBool = do 
    x <- string "true" <|> string "false"

    return $ case x of 
        "true"  -> LuaBool True 
        "false" -> LuaBool False
        _ -> undefined

var :: Parser Expr 
var = do 
    first <- char '$' *> letterChar 
    rest  <- many (char '.' <|> alphaNumChar)

    return $ Var (first : rest)

varString :: Parser String 
varString = do 
    Var v <- var
    return v

getter :: Parser Expr 
getter = Get <$> parseGetter

emptyTable :: Parser Expr 
emptyTable = string "{}" $> EmptyTable
