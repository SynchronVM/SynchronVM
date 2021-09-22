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
    ( compile
    ) where

import Parser.AbsTinyCamiot
import Desugaring.AST
import Parser.PrintTinyCamiot

import qualified Parser.Parser as P
import qualified Parser.Preprocessor as PreP
import qualified Data.Text.IO as T
import Text.Megaparsec
import Data.List

import Typechecker.TypecheckTinyCamiot
import Typechecker.Substitution

import qualified Renaming.Renaming as R
import qualified LambdaLifting.LambdaLifting as L
import qualified Monomorphisation.Monomorphise as M
import qualified Desugaring.Desugar as D
import qualified Interpreter.Interpreter as I
import qualified DetectRecs.DetectRecs as DR

import Debug.Trace

compile :: String -> IO (Either String (SExp SType))
compile input = do
    contents <- T.readFile input

    {- PreP.process adding layout tokens.
       - May be the culprit in the whitespace issues -}
    let processed = PreP.process contents
    let parsed = Text.Megaparsec.parse P.pProgram input processed
    putStrLn "\n\n"
    putStrLn "Parser output"
    putStrLn $ show parsed
    putStrLn "\n\n"
    case parsed of
        Left err   -> return $ Left (errorBundlePretty err)
        Right defs -> do
            tc <- typecheck defs
            case tc of
                Left err2           -> return $ Left (show err2)
                Right (tree, subst) -> do let (rn, state1) =  R.rename (apply subst tree)
                                          -- putStrLn "***** Alpha Renamed version *****"
                                          -- putStrLn $ betterPrint rn
                                          -- putStrLn ""
                                          let (ll, state2) =  L.lambdaLift rn state1
                                          -- putStrLn "***** LambdaLifted version *****"
                                          -- putStrLn $ betterPrint ll
                                          -- putStrLn ""
                                          (mm2, state3) <- M.monomorphise state2 ll
                                          -- putStrLn "***** Monomorphized version 2 *****"
                                          -- putStrLn $ betterPrint mm2
                                          -- putStrLn ""
                                          let ss           =  D.desugar state3 mm2
                                          putStrLn "***** Desugared version *****"
                                          putStrLn $ printTree ss
                                          putStrLn ""
                                          -- let ss'          =  DR.detectRecs ss
                                          -- putStrLn "***** Desugared recursive version *****"
                                          -- putStrLn $ printTree ss'
                                          -- putStrLn ""

                                          return $ Right ss

betterPrint :: (Show a, Print a) => [Def a] -> String
betterPrint defs = intercalate "\n\n" (map printTree groups)
  where
        f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
        f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
        f _ _                                       = False

        groups = groupBy f defs
