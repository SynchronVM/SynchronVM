
import Lib
import Interpreter.Interpreter

import System.Directory

import Data.List

path = "testcases/"

main :: IO ()
main = do
    putStrLn "********** Running tests **********"
    testcases <- readPairs path
    res <- mapM runTest testcases
    let (good, bad) = partition (== True) res
    putStrLn   "* Summary:"
    putStrLn $ "  - good tests: " ++ show (length good)
    putStrLn $ "  - bad tests:  " ++ show (length bad)

readPairs :: FilePath -> IO [(FilePath, FilePath)]
readPairs filepath = do
    files <- getDirectoryContents filepath
    return $ createPairs filepath files

createPairs :: String -> [String] -> [(String, String)]
createPairs path []     = []
createPairs path (x:xs) = case reverse x of
    ('m':'a':'c':'.':prefix) -> (path ++ x, path ++ reverse prefix ++ ".out") : createPairs path xs
    _                        -> createPairs path xs

runTest :: (String, String) -> IO Bool
runTest (input, output) = do
    putStrLn $ "* Running test: " ++ input
    expected <- readFile output
    compiled <- compile input
    case compiled of
        Left _ -> do putStrLn "FAIL: failed to compile program\n"
                     return False
        Right program -> do actual <- interpret program
                            if expected == actual
                                then putStrLn "SUCCESS\n" >> return True
                                else do putStrLn $ "FAIL:" ++
                                                   "\n  - expected output: " ++ expected ++
                                                   "\n  - actual output:   " ++ actual ++ "\n"
                                        return False