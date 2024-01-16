module TurASM.Writer.Prelude.Func (compareFunc, createFuncs) where

import Control.Monad.Writer

import TurASM.Writer.Prelude.Var

createFuncs :: Writer String () 
createFuncs = createCompareFunc

compareFunc :: String 
compareFunc = "__turasm_cmp_func"

createCompareFunc :: Writer String ()
createCompareFunc = do 
    tell ("local function " <> compareFunc <> "(a, b)\n")
    tell ("  " <> compareVarAttr "eq" <> " = a == b\n")
    tell ("  " <> compareVarAttr "neq" <> " = a ~= b\n")
    tell "  if type(a) == \"number\" and type(b) == \"number\" then\n"
    tell ("    " <> compareVarAttr "lt" <> " = a < b\n")
    tell ("    " <> compareVarAttr "gt" <> " = a > b\n") 
    tell ("    " <> compareVarAttr "le" <> " = a <= b\n") 
    tell ("    " <> compareVarAttr "ge" <> " = a >= b\n")
    tell "else\n"
    tell ("    " <> compareVarAttr "lt" <> " = false\n") 
    tell ("    " <> compareVarAttr "gt" <> " = false\n") 
    tell ("    " <> compareVarAttr "le" <> " = false\n") 
    tell ("    " <> compareVarAttr "ge" <> " = false\n")
    tell "  end\n" >> tell "end\n\n"
