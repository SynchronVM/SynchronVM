module Lib
    ( readAndParse
    ) where

import AbsTinyCamiot
import ParseTinyCamiot
import TypecheckTinyCamiot

readAndParse :: String -> IO (Either String [Def (Type ())])
readAndParse input = do
    contents <- readFile input
    let parsed = parse contents
    case parsed of
        Left err   -> return (Left err)
        Right defs -> typecheck defs