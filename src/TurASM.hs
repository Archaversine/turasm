module TurASM (transpile) where

import Control.Monad (when)

import TurASM.Parser
import TurASM.Writer

import System.Directory

transpile :: FilePath -> FilePath -> IO ()
transpile input output = do 
    outputExists <- doesFileExist output

    when outputExists $ putStrLn $ "Overriding " <> output <> "..." 

    parseFile input >>= writeToFile output
    putStrLn $ "Compiled " <> input <> " into " <> output <> "."
