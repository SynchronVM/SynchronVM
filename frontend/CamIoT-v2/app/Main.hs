module Main where

import Compile

import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> compile fp
        _    -> do
            putStrLn "not implemented yet :D :D some fancy error message please?"
            exitFailure
