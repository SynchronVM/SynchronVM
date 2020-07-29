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
   = ACC   Int  -- access the nth component of the environment register
   | QUOTE Sys  -- load immediate i.e `li` from the code area to environment
   | PUSH       -- copy content of register to stack
   | SWAP       -- interchange between register and topmost element of stack
   | CUR Label  -- build a closure with labeled exp and the environment register
   | RETURN     -- return from a subroutine call
   | APP
   deriving (Ord, Show, Eq)

type Label = String -- labels to identify a subroutine







-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register

-}
