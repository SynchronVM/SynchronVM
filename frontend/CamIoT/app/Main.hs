-- MIT License

-- Copyright (c) 2020,2021 Robert Krook and Joel Svensson

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

import Control.Monad

import Numeric
import Data.List
import Data.Maybe
import GHC.Word

import Bytecode

data Target = Target { inputFile :: Maybe FilePath
                     , outputFile :: Maybe String
                     , verbose :: Bool
                     -- Add more stuff as needed 
                     }
              deriving (Eq, Ord, Show)
emptyTarget :: Target
emptyTarget = Target { inputFile = Nothing
                     , outputFile = Nothing
                     , verbose = False }


helpText :: String
helpText = unlines
 ["camiotc"
 ,"  Options"
 ,"    -o <output>  : Specify output file"
 ,"    --verbose    : Compiler becomes very chatty"
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
parseOption__ t ("--verbose":ss) = Right (ss, t { verbose = True })
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

foreign_c_arr :: [(String, Word8)] -> String
foreign_c_arr foreigns = intercalate ", " $ map (flip (++) "_trampoline" . fst) foreigns

foreign_c_trampolines :: [(String, Word8)] -> [String]
foreign_c_trampolines foreigns = map singleForeign foreigns
  where
    singleForeign :: (String, Word8) -> String
    singleForeign (name, arity) =
      let argindexes = map (flip (-) 1) [1..arity]
          args = map (\i -> "args[" ++ show i ++ "]") argindexes
      in unlines [ "inline cam_value_t " ++ name ++ "_trampoline(cam_value_t *args) {"
                 , "    return " ++ name ++ "(" ++ intercalate ", " args ++ ");"
                 , "}"
                 ]

tag_table_c_compare :: [(String, Word16)] -> String
tag_table_c_compare contents =
  unlines [ "bool is_constructor(uint16_t tagidx, char *constr) {"
          , "    switch(tagidx) {"
          , intercalate "\n" $ map single_case contents
          , "        default: return false;"
          , "    }"
          , "    return false;"
          , "}"
          ]
  where
    single_case :: (String, Word16) -> String
    single_case (tag, index) =
      unlines [ concat ["        case ", show index, ": {"]
              , concat ["            return strncmp(constr, ", show tag, ", strlen(constr)) == 0;"]
              , "            break;"
              , "        }"
              ]

doCompile :: Target -> IO ()
doCompile t
  | (inputFile t) == Nothing = putStrLn "No input file specified"
  | otherwise =
      do
        let input = fromJust (inputFile t)
        let (outFile, foreignOutFile, constructorCompareFunction, trampolinefile) = case outputFile t of
                        Nothing -> ("out.svm", "out.svmarr", "out.constr", "out.h")
                        Just s -> (s ++ ".svm", s ++ ".svmarr", s ++ ".c", s ++ "trampolines.h")
        putStrLn $ "compiling file " ++ show input ++ " to output " ++ show outFile
    
        (compiled, foreign_arr, constructor_table) <- byteCompile (verbose t) input
        let bc = concat $ intersperse ", " $ hexStrings compiled
        condPutStrLn (verbose t) $ "SenseVM ByteCode: \n" ++ bc
        writeFile outFile bc
        when (not $ null foreign_arr) $ do
          writeFile foreignOutFile (foreign_c_arr foreign_arr)
          writeFile constructorCompareFunction (tag_table_c_compare constructor_table)
          writeFile trampolinefile (unlines $ foreign_c_trampolines foreign_arr)
                                         
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
