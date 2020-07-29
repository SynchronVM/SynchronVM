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

module CAM where

type Var = String

data Exp = Var Var  -- variable
         | Sys Sys  -- Primops
         | Void     -- Empty Tuple
         | Pair Exp Exp    -- Pair
         | Con  Exp        -- Constructed Value
         | App Exp Exp     -- Function application
         | Lam Pat Exp     -- Lambda Abstraction
         | If Exp Exp Exp  -- if then else
         | Let Pat Exp Exp -- Let bindings
         | Letrec Pat Exp Exp -- letrec
         | Case Exp [(Exp, Exp)]
         --            ^    ^
         --     constructor |
         --              resulting expression
         deriving (Ord, Show, Eq)

data Sys = Sys2 Exp Exp -- BinOp
         | Sys1 Exp     -- UnaryOp
         | LInt Int     -- Int s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         deriving (Ord, Show, Eq)

data Pat = PatVar Var
         | Empty
         | PatPair Pat Pat
         | As Var Pat      -- var `as` pat; equivalent to @ in Haskell
         deriving (Ord, Show, Eq)

data Instructions
   = -- ACCESS INSTRUCTIONS
     FST      -- access the left component of the environement register
   | SND      -- access the right component of the environement register
   | ACC  Int -- ACC n  := FST^n;SND
   | REST Int -- REST n := FST^n

     -- STACK OPERATIONS
   | PUSH       -- copy content of register to stack
   | SWAP       -- interchange between register and topmost element of stack

     -- REGISTER OPERATIONS
   | QUOTE Sys  -- QUOTE S(0) load immediate i.e `li` from the code area to environment
   | CLEAR      -- clear environment register
   | PRIM Sys   -- if PRIM s(1) then apply primop to env reg
                -- if PRIM s(2) then apply primop to top of stack (arg1) and env reg(arg2)
   | CONS       -- join (stack_top, env_reg) and place it on environment register
   | CUR Label  -- build a closure with labeled exp and the environment register
   | PACK Val   -- create tagged value with (VCon pack_val env_reg) and place on env_reg

     -- CONTROL INSTRUCTIONS
   | SKIP
   | STOP
   | APP
   | RETURN     -- return from a subroutine call
   | CALL Label
   | GOTOFALSE Label
   | SWITCH [Val] -- case expression for constructors
   | GOTO Label
   deriving (Ord, Show, Eq)

type Label = String -- labels to identify a subroutine

-- Val is basically Weak Head Normal Form
data Val = VInt  Int  -- constants s(0)
         | VBool Bool -- constants s(0)
         | VEmpty     -- empty tuple
         | VPair Val Val -- Pair
         | VCon Val Val  -- first argument is the tag second is the rest of the value
         | VClosure Val Label -- closure; Val is the environment
         | VComb Label        -- closure of a combinator; no free variables
         deriving (Ord, Show, Eq)

type Stack = [Val]

type EnvReg = Val -- the environment register

data Env a = EnvEmpty                 -- empty environment
           | EnvPair (Env a) Pat      -- constructed environment
           | EnvAnn  (Env a) (a, Pat) -- annotated environment
           deriving (Ord, Show, Eq)

data CAM = Ins Instructions -- instructions
         | Seq CAM CAM      -- sequence
         | Lab Label CAM    -- labeled sequence
         deriving (Ord, Show, Eq)

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
-}
