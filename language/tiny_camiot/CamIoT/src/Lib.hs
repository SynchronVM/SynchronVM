module Lib
    ( readAndParse
    ) where

import AbsTinyCamiot
import ParseTinyCamiot
import TypecheckTinyCamiot

readAndParse :: String -> IO (Either String [Def ()])
readAndParse input = do
    contents <- readFile input
    return $ parse contents >>= typecheck