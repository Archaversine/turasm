module TurASM.Writer.Prelude.Var ( createLocalsAndVars 
                                 , compareVarAttr 
                                 , mainLabel 
                                 , exitLabel
                                 , jumpVar
                                 , statusVar
                                 , localMapVar
                                 , toVarString
                                 , toLocalVarString
                                 ) where

import Control.Monad.Writer

import Data.List.Split

createLocalsAndVars :: Writer String () 
createLocalsAndVars = do 
    createLocals 
    createVars

createLocals :: Writer String () 
createLocals = do 
    createLocal localMapVar "{}"
    createLocal mainLabel (show mainLabel) 
    createLocal exitLabel (show exitLabel) 
    createLocal jumpVar (show mainLabel) 

    createCompareVar

-- Create an actual local variable
createLocal :: String -> String -> Writer String () 
createLocal name value = tell ("local " <> name <> " = " <> value <> "\n")

-- Create a variable in the TurASM var table
createVar :: String -> String -> Writer String () 
createVar name value = tell (localMapVar <> "[\"" <> name <> "\"] = " <> value <> "\n")

createVars :: Writer String ()
createVars = do 
    createVar statusVar "false"

prefix :: String -> String
prefix = ("__turasm_" <>)

statusVar :: String 
statusVar = "status"

compareVar :: String
compareVar = prefix "cmp"

compareVarAttr :: String -> String 
compareVarAttr attribute = compareVar <> "." <> attribute

createCompareVar :: Writer String ()
createCompareVar = do 
    tell ("local " <> compareVar <> " = {\n")
    tell "  eq = false,\n"
    tell "  neq = false,\n"
    tell "  lt = false,\n"
    tell "  gt = false,\n"
    tell "  le = false,\n"
    tell "  ge = false,\n"
    tell "}\n\n"

jumpVar :: String 
jumpVar = prefix "jmp"

-- Table for storing all variables 
-- referenced in the program
localMapVar :: String 
localMapVar = prefix "var_table"

mainLabel :: String 
mainLabel = prefix "main"

exitLabel :: String 
exitLabel = prefix "exit"

toVarString :: String -> String 
toVarString xs = concat $ take 1 parts <> map toIndex (drop 1 parts) 
  where parts = splitOn "." xs
        toIndex x = "[\"" <> x <> "\"]"

toLocalVarString :: String -> String 
toLocalVarString xs = toVarString (localMapVar <> "." <> xs)
