-- MIT License

-- Copyright (c) 2021 Abhiroop Sarkar

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

module Peephole (optimise) where

import CamOpt
import qualified Control.Monad.State.Strict as S

data Code = Code { instrs :: [FlatCAM]
                 , programcounter :: Int
                 } deriving Show

newtype Optimise a =
  Optimise
    { runOptimise :: S.State Code a
    }
  deriving (Functor, Applicative, Monad, S.MonadState Code)

optimise :: CAM -> CAM
optimise c = undefined
  where
    flatcam = flattenCAM c

optimiser :: Optimise ()
optimiser = do
  pc     <- S.gets programcounter
  is     <- S.gets instrs
  if (pc == length is - 1)
  then pure ()
  else do
    let i = is !! pc
    case i of
      (Plain (Ins i_)) -> do
          let (is_, offset) = oneOPRule i_
          let instrs_ = take pc is ++ map flatcaminst is_ ++ drop pc is
          S.modify $ \s -> s {instrs = instrs_}
          S.modify $ \s -> s {programcounter = pc + offset}
          optimiser
      _ -> do
        S.modify $ \s -> s {programcounter = pc + 1}
        optimiser
   where
     flatcaminst :: Instruction -> FlatCAM
     flatcaminst i = Plain (Ins i)


type Offset = Int

oneOPRule :: Instruction -> ([Instruction], Offset)
oneOPRule SKIP     = ([], -1)
oneOPRule (REST 0) = ([], -1)
oneOPRule (REST 1) = ([FST], -1)
oneOPRule (ACC  0) = ([SND], -1)
oneOPRule i        = ([i]  ,  1)

twoOPRule :: Instruction -> Instruction -> ([Instruction], Offset)
twoOPRule FST  FST  = ([REST 2],   0)
twoOPRule FST  SND  = ([ACC 1] ,   1)
twoOPRule (REST n) FST
  | n >= 2    = ([REST (n + 1)] ,  0)
  | otherwise = ([(REST n), FST],  1)
twoOPRule (REST n) SND
  | n >= 2    = ([ACC n]        ,  1)
  | otherwise = ([(REST n), SND],  1)
twoOPRule PUSH SWAP = ([PUSH]   ,  0)
twoOPRule MOVE POP  = ([]       , -1)
twoOPRule SWAP CONS = ([SNOC]   , -1)
twoOPRule SWAP SNOC = ([CONS]   , -1)

twoOPRule (CUR l)  APP = ([SNOC, CALL l], -1)
twoOPRule (COMB l) APP = ([POP , CALL l], -1)
twoOPRule (CALL l) RETURN = ([GOTO l], 1)
twoOPRule i1       i2     = ([i1, i2], 1)





data FlatCAM = Plain CAM
             | Labeled Label CAM
             deriving (Ord, Show, Eq)

flattenCAM :: CAM -> [FlatCAM]
flattenCAM (Ins i) = [Plain $ Ins i]
flattenCAM (Seq c1 c2) = flattenCAM c1 ++ flattenCAM c2
flattenCAM (Lab l c1)  =
  case (head f) of
    (Plain (Ins instr)) -> Labeled l (Ins instr) : tail f
    _ -> error "Failure in flattening"
  where
    f = flattenCAM c1

rebuildCAM :: [FlatCAM] -> CAM
rebuildCAM [] = error "Failure when rebuilding CAM"
rebuildCAM [(Plain c)] = c
rebuildCAM [(Labeled l c)] = Lab l c
rebuildCAM (x:xs) =
  case x of
    Plain c -> Seq c (rebuildCAM xs)
    Labeled l c -> Seq (Lab l c) (rebuildCAM xs)

-- The `show` is necessary because `rebuildCAM` simplifies the
-- structure of the CAM tree made of `Seq`. `rebuildCAM` essentially
-- makes CAM a fully right leaning linked list. The original
-- CAM data type might have had some more complex structure.
verify :: CAM -> Bool
verify c = (show . rebuildCAM . flattenCAM) c == (show c)





example33 =
  Let (PatVar "foo") (Lam (PatVar "x") (Sys $ Sys2 PlusI (Var "x") eleven))
  (Let (PatVar "r") two
   (App (Var "foo") (Var "r")))
  where
    two  = Sys $ LInt 2
    four = Sys $ LInt 4
    eleven = Sys $ LInt 11
