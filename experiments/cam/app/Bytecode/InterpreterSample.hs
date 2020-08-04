-- MIT License

-- Copyright (c) 2020 Abhiroop Sarkar

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

{-# LANGUAGE RecordWildCards #-}

module Bytecode.InterpreterSample where

import CAM
import GHC.Arr
import qualified Control.Monad.State.Strict as S

{-
NOTE: This is not the actual "byte"code interpreter.
This takes the Haskell `Instruction` datatype and interprets that.
This module would be primarily used for experimentation with VM designs

NOTE 2: Efficiency of the data structures like symbol table
doesn't matter because we will generate more efficient C types
-}

type Stack = [Val]

type EnvReg = Val -- the environment register

type Index = Int

type SymbolTable = [(Label, Index)]

data Code = Code { instrs :: Array Index Instruction
                 , symbolTable :: SymbolTable
                 , prevJump    :: [Index]
                 , programCounter :: Index
                 } deriving Show

newtype Evaluate a =
  Evaluate
    { runEvaluate :: S.State Code a
    }
  deriving (Functor, Applicative, Monad, S.MonadState Code)


-- Val is basically Weak Head Normal Form
data Val = VInt  Int  -- constants s(0)
         | VBool Bool -- constants s(0)
         | VEmpty     -- empty tuple
         | VPair Val Val -- Pair
         | VCon Tag Val  -- first arg is the tag second is value with tag
         | VClosure Val Label -- closure; Val is the environment
         | VComb Label        -- closure of a combinator; no free variables
         deriving (Ord, Show, Eq)


-- Take a sequence of stack machine instructions
-- and evaluate them to their normal form
evaluate :: CAM -> Val
evaluate cam = val
  where
    code = initCode cam
    val  = S.evalState (runEvaluate $ eval VEmpty []) code

initCode :: CAM -> Code
initCode cam = Code { instrs = listArray (1, totalInstrs) caminstrs
                    , symbolTable = filteredEntries
                    , prevJump    = []
                    , programCounter = 1
                    }
  where
    instrsLabs = genInstrs cam dummyLabel
    indexedinstrsLabs = zip instrsLabs [1..]
    caminstrs = map (fst . fst) indexedinstrsLabs
    entries   = map (\((_,l),idx) -> (l,idx)) indexedinstrsLabs
    filteredEntries = filter (\(l,_) -> l /= dummyLabel) entries
    totalInstrs     = length caminstrs

genInstrs :: CAM -> Label -> [(Instruction, Label)]
genInstrs (Ins i) l = [(i, l)]
genInstrs (Seq c1 c2) l = genInstrs c1 l ++ genInstrs c2 dummyLabel
genInstrs (Lab l c) _   = genInstrs c l

eval :: EnvReg -> Stack -> Evaluate Val
eval envreg stack = return envreg


-- In this case we have a common error message
-- because this operation is only on a symbol table
(~>) :: SymbolTable -> Label -> Index
(~>) [] _ = error "Label missing in symbol table"
(~>) ((label,idx):st) l
  | l == label = idx
  | otherwise  = st ~> l

put :: SymbolTable -> Label -> Index -> SymbolTable
put st l idx = (l,idx) : st


emptyST :: SymbolTable
emptyST = []

dummyLabel = "dummy"

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
-}
