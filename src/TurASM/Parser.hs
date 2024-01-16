module TurASM.Parser (parseFile) where

import Text.Megaparsec

import TurASM.Parser.Instruction (parseInstructions)
import TurASM.Parser.Types (Instruction)

parseFile :: FilePath -> IO [Instruction]
parseFile path = do 
    content <- readFile path 

    case parse parseInstructions path content of 
        Left err -> do 
            putStrLn $ errorBundlePretty err 
            errorWithoutStackTrace "Failed to parse file"
        Right x  -> return x
