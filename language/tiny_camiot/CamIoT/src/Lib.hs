module Lib
    ( readAndParse
    ) where

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
                Right tc -> print tc >> return (Right tc)