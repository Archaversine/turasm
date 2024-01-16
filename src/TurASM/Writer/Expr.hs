{-# LANGUAGE LambdaCase #-}

module TurASM.Writer.Expr (writeExpr) where

import Control.Monad.Writer 

import Data.Char (toLower)

import TurASM.Parser.Types
import TurASM.Writer.Prelude

writeExpr :: Expr -> Writer String ()
writeExpr = \case 
    Number x    -> writeNumber x
    LuaString x -> writeString x
    LuaList xs  -> writeList xs
    LuaBool x   -> writeBool x
    Var x       -> writeVar x
    Add a b     -> writeBinOp '+' a b
    Sub a b     -> writeBinOp '-' a b 
    Mul a b     -> writeBinOp '*' a b 
    Div a b     -> writeBinOp '/' a b 
    Mod a b     -> writeBinOp '%' a b
    Get g       -> writeGetter g
    EmptyTable  -> writeEmptyTable

writeNumber :: String -> Writer String ()
writeNumber = tell

writeString :: String -> Writer String () 
writeString x = tell ("\"" <> x <> "\"")

writeList :: [Expr] -> Writer String ()
writeList xs = do 
    tell "{" 

    forM_ xs $ \x -> do 
        writeExpr x 
        tell ","

    tell "}"

writeBool :: Bool -> Writer String () 
writeBool = tell . map toLower . show

writeVar :: String -> Writer String () 
writeVar = tell . toLocalVarString

writeBinOp :: Char -> Expr -> Expr -> Writer String () 
writeBinOp c e1 e2 = do 
    tell "("
    writeExpr e1 
    tell [c]
    writeExpr e2
    tell ")"

writeGetter :: Getter -> Writer String ()
writeGetter g = tell ("turtle.get" <> show g <> "()")

writeEmptyTable :: Writer String () 
writeEmptyTable = tell "{}"
