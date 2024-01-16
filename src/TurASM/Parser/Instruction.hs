module TurASM.Parser.Instruction (parseInstruction, parseInstructions) where

import Control.Monad (unless)

import Data.Functor

import Prelude hiding (Left, Right)

import TurASM.Parser.Types 
import TurASM.Parser.Expr

import Text.Megaparsec
import Text.Megaparsec.Char

parseInstructions :: Parser [Instruction]
parseInstructions = space *> many parseInstruction <* errorOnRest

-- Fail if there are any non whitespace characters left to parse
errorOnRest :: Parser () 
errorOnRest = do 
    rest <- many (hspace *> some (noneOf " \t\n\r"))
    unless (null rest) $ fail "Invalid Instruction (Did you mispell something?)"

parseInstruction :: Parser Instruction 
parseInstruction = try mov
               <|> try (insWithDir TUR "tur")
               <|> try (insWithExpr SEL "sel") 
               <|> try (insWithDir EQU "equ")
               <|> try (insWithDir ATK "atk") 
               <|> try (insWithDir ATU "atu") 
               <|> try (insWithDir ATD "atd") 
               <|> try (insWithDir DIG "dig") 
               <|> try (insWithDir DGU "dgu") 
               <|> try (insWithDir DGD "dgd") 
               <|> try (insWithDir PLC "plc") 
               <|> try psn 
               <|> try (insWithExpr DRP "drp") 
               <|> try (insWithExpr DPU "dpu") 
               <|> try (insWithExpr DPD "dpd") 
               <|> try (insWithExpr SCK "sck") 
               <|> try (insWithExpr SKU "sku") 
               <|> try (insWithExpr SKD "skd") 
               <|> try (insWithExpr REF "ref") 
               <|> try tra 
               <|> try (insWithVarExpr SET "set") 
               <|> try (insWithVarExpr ADD "add") 
               <|> try (insWithVarExpr SUB "sub") 
               <|> try (insWithVarExpr MUL "mul") 
               <|> try (insWithVarExpr DIV "div") 
               <|> try (insWithVarExpr MOD "mod") 
               <|> try cmp 
               <|> try (insWithIdentifier JMP "jmp") 
               <|> try (insWithIdentifier JEQ "jeq") 
               <|> try (insWithIdentifier JNE "jne") 
               <|> try (insWithIdentifier JLT "jlt") 
               <|> try (insWithIdentifier JGT "jgt") 
               <|> try (insWithIdentifier JLE "jle") 
               <|> try (insWithIdentifier JGE "jge")
               <|> try (insWithExpr JBL "jbl")
               <|> try (insWithExpr SAY "say")
               <|> try section
               <|> comment

lineEnd :: Parser ()
lineEnd = hspace *> void (some eol) <|> eof

identifier :: Parser String 
identifier = (:) <$> letterChar <*> many alphaNumChar

direction :: Parser Direction 
direction = choice [ string "up"      $> Up
                   , string "down"    $> Down 
                   , string "left"    $> Left 
                   , string "right"   $> Right
                   , string "forward" $> Forward
                   , string "back"    $> Back
                   , string "*"       $> Any
                   ]

insWithDir :: (Direction -> Instruction) -> String -> Parser Instruction
insWithDir ins name = ins <$> (hspace *> string' name *> hspace1 *> direction <* lineEnd)

insWithExpr :: (Expr -> Instruction) -> String -> Parser Instruction 
insWithExpr ins name = ins <$> (hspace *> string' name *> hspace1 *> parseExpr <* lineEnd)

insWithIdentifier :: (String -> Instruction) -> String -> Parser Instruction 
insWithIdentifier ins name = ins <$> (hspace *> string' name *> hspace1 *> identifier <* lineEnd)

insWithVarExpr :: (String -> Expr -> Instruction) -> String -> Parser Instruction 
insWithVarExpr ins name = do 
    hspace *> string' name *> hspace1

    s <- varString <* hspace1
    e <- parseExpr <* lineEnd

    return (ins s e)

mov :: Parser Instruction 
mov = do 
    e <- hspace *> string' "mov" *> hspace1 *> parseExpr
    d <- hspace1 *> direction <* lineEnd

    return (MOV e d)

-- Place Sign
psn :: Parser Instruction
psn = do 
    hspace *> string' "psn" *> hspace1

    d <- hspace1 *> direction <* hspace1 
    s <- char '"' *> manyTill anySingle (char '"') <* lineEnd

    return (PSN d s)

-- Transport 
tra :: Parser Instruction 
tra = do 
    hspace *> string' "tra" *> hspace1 

    e1 <- parseExpr <* hspace1 
    e2 <- parseExpr <* lineEnd

    return (TRA e1 e2)

cmp :: Parser Instruction 
cmp = do 
    hspace *> string' "cmp" *> hspace1 

    e1 <- parseExpr <* hspace1 
    e2 <- parseExpr <* lineEnd 

    return (CMP e1 e2)

section :: Parser Instruction 
section = do 
    first <- letterChar
    rest  <- manyTill alphaNumChar (char ':') <* lineEnd

    return (Section (first : rest))

comment :: Parser Instruction 
comment = Comment <$> (space *> char ';' *> manyTill anySingle (some eol))
