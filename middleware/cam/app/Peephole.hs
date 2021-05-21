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

import Debug.Trace

optimise :: CAM -> CAM
optimise cam = cam'
  where
    flatcam   = flattenCAM cam
    initState = initCode flatcam 0
    flatcam'  = S.evalState (runOptimise optimiser) initState
    cam'      = rebuildCAM flatcam'

data Code = Code { instrs :: [FlatCAM]
                 , programcounter :: Int
                 } deriving Show

initCode :: [FlatCAM] -> Int -> Code
initCode is pc = Code { instrs = is
                      , programcounter = pc
                      }
newtype Optimise a =
  Optimise
    { runOptimise :: S.State Code a
    }
  deriving (Functor, Applicative, Monad, S.MonadState Code)



optimiser :: Optimise [FlatCAM]
optimiser = do
  pc <- getPC
  b  <- terminateNow
  if b
  then do
    is <- getInstrs
    pure is
  else do
    o <- applyTwoOPRule
    incPCBy o
    o'<- applyOneOPRule
    incPCBy o'
    optimiser

applyOneOPRule :: Optimise Offset
applyOneOPRule = do
  is <- getInstrs
  pc <- getPC
  b  <- atleastNInstrs 1
  if b
  then do
    let i1 = is !! pc
    case i1 of
      Plain (Ins i1_) -> do
          let (is_, offset) = oneOPRule i1_
          replaceNInstrs 1 is_
          pure offset

      _ -> pure 1 -- this is because the two op rule has already been applied

  else pure 1 -- trigger `terminateNow`


applyTwoOPRule :: Optimise Offset
applyTwoOPRule = do
  is <- getInstrs
  pc <- getPC
  b  <- atleastNInstrs 2
  if b
  then do
    let i1 = is !! pc
    let i2 = is !! (pc + 1) -- the if check makes sure that PC doesnt go out of bound
    case (i1, i2) of
      (Plain (Ins i1_), Plain (Ins i2_)) -> do
          let (is_, offset) = twoOPRule i1_ i2_
          replaceNInstrs 2 is_
          pure offset

      _ -> pure 0 -- maybe the OneOPRule applies so don't increment PC

  else pure 0 -- if there is only one instruction and applyOneOPRule applies

incPCBy :: Offset -> Optimise ()
incPCBy offset = do
  pc <- getPC
  S.modify $ \s -> s {programcounter = pc + offset}

replaceNInstrs :: Int -> [Instruction] -> Optimise ()
replaceNInstrs n is_ = do
  is <- getInstrs
  pc <- getPC
  let instrs_ = take pc is
             ++ map flatcaminst is_
             ++ drop (pc + n) is
  S.modify $ \s -> s {instrs = instrs_}
  where
    flatcaminst :: Instruction -> FlatCAM
    flatcaminst i = Plain (Ins i)

terminateNow :: Optimise Bool
terminateNow = do
  pc <- getPC
  is <- getInstrs
  pure $ (pc > (length is - 1))

atleastNInstrs :: Int -> Optimise Bool
atleastNInstrs n = do
  pc <- getPC
  is <- getInstrs
  pure $ (pc + n <= length is)

getPC :: Optimise Int
getPC = do
  pc <- S.gets programcounter
  pure pc

getInstrs :: Optimise [FlatCAM]
getInstrs = do
  is <- S.gets instrs
  pure is

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
  | otherwise = ([(REST n), FST],  0)
twoOPRule (REST n) SND
  | n >= 2    = ([ACC n]        ,  1)
  | otherwise = ([(REST n), SND],  0)
twoOPRule PUSH SWAP = ([PUSH]   ,  0)
twoOPRule MOVE POP  = ([]       , -1)
twoOPRule SWAP CONS = ([SNOC]   , -1)
twoOPRule SWAP SNOC = ([CONS]   , -1)

twoOPRule (CUR l)  APP = ([SNOC, CALL l], -1)
twoOPRule (COMB l) APP = ([POP , CALL l], -1)
twoOPRule (CALL l) RETURN = ([GOTO l], 1)
twoOPRule i1       i2     = ([i1, i2], 0) -- try oneOPRules now





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
