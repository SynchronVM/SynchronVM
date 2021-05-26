-- MIT License

-- Copyright (c) 2020 Robert Krook and Joel Svensson

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
module Main where

import System.Environment (getArgs)
import System.Exit

import Numeric
import Data.List
import GHC.Word

import Lib (compile)
import Bytecode

data Target = Target { inputFile :: Maybe FilePath
                     , outputFile :: Maybe FilePath
                     -- Add more stuff as needed 
                     }
              deriving (Eq, Ord, Show)
emptyTarget :: Target
emptyTarget = Target Nothing Nothing


helpText :: String
helpText = unlines
 ["camiotc"
 ,"  Options"
 ,"    -o <output>  : Specify output file"
 ,"    --help       : Prints this message"
 ," "
 ,"  Usage: "
 ,"    camiotc input-file.cam -o output.x" ] 

parseOption_ :: Target -> [String] -> Either String ([String], Target)
parseOption_ t [] = Right ([],t)
parseOption_ t ("-o":(s:ss)) = case outputFile t of
                               Just _  -> Left "Error: More than one output specified"
                               Nothing -> Right (ss, t { outputFile = Just s} )
parseOption_ t s = Left $ "Error: Incorrect option " ++ show s

parseOption__ :: Target -> [String] -> Either String ([String], Target)
parseOption__ t [] = Right ([], t)
parseOption__ t ("--help":ss) = Left helpText
parseOption__ t s = Left $ "Error: Incorrect option " ++ show s

parseArg :: Target -> [String] -> Either String ([String] ,Target)
parseArg t [] = Right ([], t)
parseArg t ss@(s:_)
  | isPrefixOf "--" s  = case parseOption__ t ss of
                           Left s -> Left s
                           Right (ss, t) -> parseArg t ss
  | isPrefixOf "-" s = case parseOption_ t ss of
                         Left s -> Left s
                         Right (ss, t) -> parseArg t ss
  | otherwise = case inputFile t of
                  Just _ -> Left "Error: more than one input files specified"
                  Nothing -> parseArg (t { inputFile = Just s }) (tail ss)

  
  

parseArgs :: IO (Maybe Target)
parseArgs = do
  args <- getArgs
  case parseArg emptyTarget args of
             Left m -> do putStrLn m
                          return Nothing
             Right (_,t) -> return $ Just t


hexStrings :: [Word8] -> [String]
hexStrings xs = map (\x -> "0x" ++ showHex x "") xs


doCompile :: Target -> IO ()
doCompile (Target Nothing _) = putStrLn "No input file specified"
doCompile (Target (Just input) output) =
  do
    let outFile = case output of
                    Nothing -> "out.svm"
                    Just s -> s
    putStrLn $ "compiling file " ++ show input ++ " to output " ++ show outFile
    
    compiled <- byteCompile input
    let bc = concat $ intersperse ", " $ hexStrings compiled
    putStrLn $ "ByteCode: " ++ bc
    writeFile outFile bc
                                         
main :: IO ()
main = do
  targ <- parseArgs
  case targ of
    Nothing -> return ()
    Just t -> doCompile t


      
  -- args <- getArgs
  -- case args of
  --   ["--help"] -> putStrLn "camiotc filename.cam"
  --   [fs] -> do
  --       res <- compile fs
  --       case res of
  --         Left err   -> putStrLn err >> exitFailure
  --         Right tree -> putStrLn (show tree) >> exitSuccess
  --   _ -> putStrLn "Right now I can only handle one input file" -- :c
