-- MIT License

-- Copyright (c) 2020 Robert Krook

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
module Lib
    ( readAndParse
    ) where

import System.Exit

import Parser.AbsTinyCamiot

import qualified Parser.Parser as P
import qualified Parser.Preprocessor as PreP
import qualified Data.Text.IO as T
import Text.Megaparsec

import Typechecker.TypecheckTinyCamiot
import Typechecker.Environment

readAndParse :: String -> IO (Either String Subst)
readAndParse input = do
    contents <- T.readFile input
    let processed = PreP.process contents
    let parsed = Text.Megaparsec.parse P.pProgram input processed
    case parsed of
        Left err   -> return (Left (show err))
        Right defs -> do
            tc <- typecheck defs
            case tc of
                Left err -> return $ Left $ show err
                Right tc -> return (Right tc)