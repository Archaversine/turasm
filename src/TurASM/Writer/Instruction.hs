{-# LANGUAGE LambdaCase #-}

module TurASM.Writer.Instruction (writeInstruction) where

import Control.Monad.Writer hiding (Any)

import Data.Char (toLower)

import Prelude hiding (Left, Right)

import TurASM.Parser.Types
import TurASM.Writer.Expr
import TurASM.Writer.Prelude

writeInstruction :: Instruction -> Writer String () 
writeInstruction = \case 

    MOV e Left  -> writeMovLeft e
    MOV e Right -> writeMovRight e
    MOV e Any   -> writeMovDir "forward" e -- Default to forward
    MOV e d     -> writeMovDir (map toLower $ show d) e

    TUR d -> writeTur d
    SEL e -> writeSel e
    EQU d -> writeEqu d

    ATK d -> writeDirectional "attack" d
    ATU d -> writeDirectional "attackUp" d
    ATD d -> writeDirectional "attackDown" d

    DIG d -> writeDirectional "dig" d 
    DGU d -> writeDirectional "digUp" d
    DGD d -> writeDirectional "digDown" d

    PLC d   -> writePlc d
    PSN d s -> writePsn s d

    DRP e -> writeDrp e
    DPU e -> writeDpu e
    DPD e -> writeDpd e

    SCK e -> writeSck e 
    SKU e -> writeSku e 
    SKD e -> writeSkd e 

    REF e -> writeRef e
    TRA e1 e2 -> writeTra e1 e2

    SET s e -> writeSet s e
    ADD s e -> writeInplace "+" s e 
    SUB s e -> writeInplace "-" s e 
    MUL s e -> writeInplace "*" s e 
    DIV s e -> writeInplace "/" s e 
    MOD s e -> writeInplace "%" s e

    CMP e1 e2 -> writeCmp e1 e2 

    JMP x -> writeJmp x
    JEQ x -> writeConditionalJump x "eq"
    JNE x -> writeConditionalJump x "neq" 
    JLT x -> writeConditionalJump x "lt" 
    JGT x -> writeConditionalJump x "gt" 
    JLE x -> writeConditionalJump x "le" 
    JGE x -> writeConditionalJump x "ge" 
    JBL x -> writeJbl x

    SAY e -> writeSay e

    Section x -> writeSection x
    Comment _ -> writeComment

setStatusString :: String -> Writer String ()
setStatusString s = setStatus (tell s)

setStatus :: Writer String a -> Writer String ()
setStatus action = do 
    tell (toLocalVarString statusVar <> " = ")
    void action
    tell "\n"

writeMovDir :: String -> Expr -> Writer String ()
writeMovDir d (Number "1") = setStatusString ("turtle." <> d <> "()")
writeMovDir d e = do 
    tell "for _=1," >> writeExpr e >> tell " do\n"
    setStatusString ("turtle." <> d <> "()")
    tell "end\n"

writeMovLeft :: Expr -> Writer String () 
writeMovLeft e = do 
    setStatusString "turtle.turnLeft()"
    writeMovDir "left" e

writeMovRight :: Expr -> Writer String ()
writeMovRight e = do 
    setStatusString "turtle.turnRight()"
    writeMovDir "right" e

writeTur :: Direction -> Writer String () 
writeTur = \case
    Left  -> setStatusString "turtle.turnLeft()"
    Right -> setStatusString "turtle.turnRight()"
    _     -> pure ()

writeSel :: Expr -> Writer String ()
writeSel e = setStatus $ tell "turtle.select(" >> writeExpr e >> tell ")"

writeEqu :: Direction -> Writer String () 
writeEqu = \case 
    Left  -> setStatusString "turtle.equipLeft()" 
    Right -> setStatusString "turtle.equipRight()" 
    _     -> pure ()

-- ATK instructions 
-- DIG instructions
writeDirectional :: String -> Direction -> Writer String () 
writeDirectional func = \case 
    Left  -> setStatusString ("turtle." <> func <> "(\"left\")")
    Right -> setStatusString ("turtle." <> func <> "(\"right\")")
    _     -> setStatusString ("turtle." <> func <> "()")

writePlc :: Direction -> Writer String () 
writePlc = \case 
    Up   -> setStatusString "turtle.placeUp()"
    Down -> setStatusString "turtle.placeDown()"
    _    -> setStatusString "turtle.place()"

writePsn :: String -> Direction -> Writer String () 
writePsn s = \case 
    Up   -> setStatusString $ "turtle.placeUp(\""   <> s <> "\")"
    Down -> setStatusString $ "turtle.placeDown(\"" <> s <> "\")"
    _    -> setStatusString $ "turtle.place(\""     <> s <> "\")"

writeDrp :: Expr -> Writer String ()
writeDrp e = setStatus $ tell "turtle.drop(" >> writeExpr e >> tell ")"

writeDpu :: Expr -> Writer String ()
writeDpu e = setStatus $ tell "turtle.dropUp(" >> writeExpr e >> tell ")"

writeDpd :: Expr -> Writer String ()
writeDpd e = setStatus $ tell "turtle.dropDown(" >> writeExpr e >> tell ")"

writeSck :: Expr -> Writer String () 
writeSck e = setStatus $ tell "turtle.suck(" >> writeExpr e >> tell ")"

writeSku :: Expr -> Writer String () 
writeSku e = setStatus $ tell "turtle.suckUp(" >> writeExpr e >> tell ")"

writeSkd :: Expr -> Writer String () 
writeSkd e = setStatus $ tell "turtle.suckDown(" >> writeExpr e >> tell ")"

writeRef :: Expr -> Writer String ()
writeRef e = setStatus $ tell "turtle.refuel(" >> writeExpr e >> tell ")"

writeTra :: Expr -> Expr -> Writer String ()
writeTra e1 e2 = setStatus $ do 
    tell "turtle.transferTo("
    writeExpr e1 >> tell ", " >> writeExpr e2
    tell ")"

writeSet :: String -> Expr -> Writer String () 
writeSet s e = tell (toLocalVarString s <> " = ") >> writeExpr e >> tell "\n"

-- SET, ADD, SUB, MUL, DIV, MOD Instructions
writeInplace :: String -> String -> Expr -> Writer String ()
writeInplace op varname e = do 
    let name = toLocalVarString varname

    tell name >> tell " = " 
    tell name >> tell " " >> tell op >> tell " "
    writeExpr e >> tell "\n"

writeCmp :: Expr -> Expr -> Writer String ()
writeCmp e1 e2 = do 
    tell (compareFunc <> "(")
    writeExpr e1 >> tell ", " >> writeExpr e2 >> tell ")\n"

-- Using `show` to add quotations arround label variable
writeJmp :: String -> Writer String ()
writeJmp label = do 
    tell (jumpVar <> " = " <> show label <> "\n")
    tell "do break end\n" -- Basically a continue statement

writeConditionalJump :: String -> String -> Writer String ()
writeConditionalJump label condition = do 
    tell ("if " <> compareVarAttr condition <> " then\n")
    writeJmp label
    tell "end\n"

writeJbl :: Expr -> Writer String () 
writeJbl e = do 
    tell (jumpVar <> " = ") >> writeExpr e >> tell "\n"
    tell "do break end\n" -- Basically a continue statement

writeSay :: Expr -> Writer String ()
writeSay e = tell "print(" >> writeExpr e >> tell ")\n"

writeSection :: String -> Writer String () 
writeSection label = do 
    tell (jumpVar <> " = " <> show label <> "\n") -- Set jump label to next section
    tell "end\n"
    tell ("if " <> jumpVar <> " == " <> show label <> " then\n")

writeComment :: Writer String () 
writeComment = pure ()
