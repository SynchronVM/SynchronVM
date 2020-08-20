module Lib
    ( readAndParse
    ) where

import System.Exit

import AbsTinyCamiot
import ParseTinyCamiot
import TypecheckTinyCamiot
import Environment

readAndParse :: String -> IO (Either String Subst)
readAndParse input = do
    contents <- readFile input
    let parsed = parse contents
    case parsed of
        Left err   -> return (Left err)
        Right defs -> do
            tc <- typecheck defs
            case tc of
                Left err -> return $ Left $ show err
                Right tc -> return (Right tc)