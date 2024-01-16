module TurASM.Writer (writeToFile) where

import Control.Monad.Writer

import TurASM.Parser.Types
import TurASM.Writer.Prelude
import TurASM.Writer.Instruction

writeProg :: [Instruction] -> Writer String ()
writeProg instructions = do 
    fileComment 
    createLocalsAndVars
    createFuncs

    sectionalize $ do 
        tell ("if " <> jumpVar <> " == " <> mainLabel <> " then\n")
        mapM_ writeInstruction instructions
        tell (jumpVar <> " = " <> exitLabel <> "\n")
        tell "end\n" -- End of last section

writeToFile :: FilePath -> [Instruction] -> IO ()
writeToFile path instructions = writeFile path $ execWriter w
    where w = writeProg instructions
