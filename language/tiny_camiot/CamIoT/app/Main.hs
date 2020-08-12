module Main where

import System.Environment (getArgs)

import Lib (readAndParse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn "no help offered :D" -- write a nice message
    [fs] -> do
        res <- readAndParse fs
        case res of
          Left err -> print err
          Right tree -> putStrLn "parse & typecheck succeeded!"
    _ -> putStrLn "Right now I can only handle one input file" -- :c