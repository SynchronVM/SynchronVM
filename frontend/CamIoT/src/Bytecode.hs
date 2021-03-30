-- MIT License

-- Copyright (c) 2020 Robert Krook, Abhiroop Sarkar

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
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Bytecode where

import Data.Int(Int32)
import Data.List
import Desugaring.AST
import GHC.Float(double2Float)
import Interpreter.Interpreter
import Lib

import qualified CAM as C
import qualified Assembler as A
import qualified Parser.AbsTinyCamiot as AST
import qualified Parser.PrintTinyCamiot as PP

translate :: SExp SType -> C.Exp
translate (SELet _ pat e1 e2) =
  C.Let (translatePat pat) (translate e1) (translate e2)
translate (SELam _ pat exp) =
  C.Lam (translatePat pat) (translate exp)
translate (SEAdd ty e1 _ e2) =
  let e1' = translate e1
      e2' = translate e2
   in C.Sys $ C.Sys2 addOp e1' e2'
   where
     addOp = case ty of
               STInt   -> C.PlusI
               STFloat -> C.PlusF
               _ -> error "Adding non int or float operation"
translate (SEMul ty e1 _ e2) =
  let e1' = translate e1
      e2' = translate e2
   in C.Sys $ C.Sys2 mulOp e1' e2'
   where
     mulOp = case ty of
               STInt   -> C.MultiplyI
               STFloat -> C.MultiplyF
               _ -> error "Multiplying non int or float operation"

translate (SEConst ty const) =
  case const of
    AST.CInt arbitraryPrecisionInt ->
      C.Sys (C.LInt (fromInteger arbitraryPrecisionInt)) -- only 32 bit Ints supported in CAM currently
    AST.CFloat double ->
      C.Sys (C.LFloat (double2Float double)) -- only single precision floats supported in CAM currently
    AST.CTrue  -> C.Sys (C.LBool True)
    AST.CFalse -> C.Sys (C.LBool False)
    AST.CNil   -> C.Void


translatePat :: SPat SType -> C.Pat
translatePat (SPNil _ ) = C.Empty
translatePat (SPVar _ (AST.Ident str))     = C.PatVar str
translatePat (SPLay _ (AST.Ident str) pat) = C.As str (translatePat pat)
translatePat (SPTup _ p1 p2)
  = C.PatPair (translatePat p1) (translatePat p2)

path = "testcases/good2.cam"

test :: IO ()
test = do
  compiled <- compile path
  case compiled of
    Left err -> putStrLn err
    Right desugaredIr -> do
      putStrLn $ PP.printTree desugaredIr
      --putStrLn $ show desugaredIr
      -- let camir = translate desugaredIr
      -- let cam   = C.interpret camir
      -- A.genbytecode cam
      -- putStrLn $ show $ A.translate $ C.interpret $ translate desugaredIr

foo =
  let v0 = \ v2 -> case v2 of
                     v1 -> v1 * 2
  in v0 5
