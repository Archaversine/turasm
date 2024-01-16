module TurASM.Parser.Types ( Direction(..) 
                           , Instruction(..)
                           , Expr(..)
                           , Getter(..)
                           , Parser
                           ) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

data Direction = Up | Down | Left | Right | Forward | Back | Any deriving Show

data Instruction = MOV Expr Direction
                 | TUR Direction
                 | SEL Expr 
                 | EQU Direction 
                 | ATK Direction 
                 | ATU Direction 
                 | ATD Direction 
                 | DIG Direction 
                 | DGU Direction 
                 | DGD Direction 
                 | PLC Direction 
                 | PSN Direction String 
                 | DRP Expr 
                 | DPU Expr 
                 | DPD Expr 
                 | SCK Expr 
                 | SKU Expr 
                 | SKD Expr 
                 | REF Expr 
                 | TRA Expr Expr 
                 | SET String Expr 
                 | ADD String Expr 
                 | SUB String Expr 
                 | MUL String Expr 
                 | DIV String Expr  
                 | MOD String Expr 
                 | CMP Expr Expr 
                 | JMP String 
                 | JEQ String 
                 | JNE String 
                 | JLT String 
                 | JGT String 
                 | JLE String 
                 | JGE String
                 | JBL Expr   
                 | SAY Expr
                 | Section String
                 | Comment String -- Used to represent code comments
                 deriving Show

data Expr = Number String
          | LuaString String
          | LuaList [Expr]
          | LuaBool Bool
          | Var String
          | Add Expr Expr
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr 
          | Mod Expr Expr 
          | Get Getter
          | EmptyTable
          deriving Show

data Getter = SelectedSlot 
            | ItemCount 
            | ItemSpace 
            | ItemDetail 
            | Detect 
            | DetectUp
            | DetectDown
            | Inspect 
            | InspectUp 
            | InspectDown
            | Compare 
            | CompareUp 
            | CompareDown
            | FuelLevel 
            | FuelLimit
            deriving Show
