module Main (main) where

import System.Environment

import TurASM

helpMessage :: IO ()
helpMessage = putStrLn "Usage: turasm <input file> [output file]"

main :: IO ()
main = do 
    args <- getArgs

    case args of 
        [input]         -> transpile input (input ++ ".lua") 
        [input, output] -> transpile input output
        _               -> helpMessage
