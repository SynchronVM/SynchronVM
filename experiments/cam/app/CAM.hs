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

import Prelude hiding (lookup)

type Var = String
type Tag = String

data Exp = Var Var  -- variable
         | Sys Sys  -- Primops
         | Void     -- Empty Tuple
         | Pair Exp Exp    -- Pair
         | Con  Tag Exp    -- Constructed Value
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

data Sys = Sys2 BinOp Exp Exp -- BinOp
         | Sys1 UnaryOp Exp     -- UnaryOp
         | LInt Int     -- Int s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         deriving (Ord, Show, Eq)


data BinOp = Plus | Multiply | Minus deriving (Ord, Show, Eq)

data UnaryOp = Abs deriving (Ord, Show, Eq)

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
   | PRIM1 UnaryOp -- PRIM s(1) then apply primop to env reg
   | PRIM2 BinOp   -- PRIM s(2) then apply primop to top of stack (arg1) and env reg(arg2)
   | CONS       -- join (stack_top, env_reg) and place it on environment register
   | CUR Label  -- build a closure with labeled exp and the environment register
   | PACK Tag   -- create tagged value with (VCon pack_val env_reg) and place on env_reg

     -- CONTROL INSTRUCTIONS
   | SKIP   -- NoOp
   | STOP   -- halt machine
   | APP    -- function applications
   | RETURN -- return from a subroutine call
   | CALL Label
   | GOTOFALSE Label
   | SWITCH [Val] -- case expression for constructors
   | GOTO Label

   | FAIL -- a meta instruction to indicate search failure
   deriving (Ord, Show, Eq)

type Label = String -- labels to identify a subroutine

-- Val is basically Weak Head Normal Form
data Val = VInt  Int  -- constants s(0)
         | VBool Bool -- constants s(0)
         | VEmpty     -- empty tuple
         | VPair Val Val -- Pair
         | VCon Label Val  -- first argument is the tag second is the rest of the value
         | VClosure Val Label -- closure; Val is the environment
         | VComb Label        -- closure of a combinator; no free variables
         deriving (Ord, Show, Eq)

type Stack = [Val]

type EnvReg = Val -- the environment register

-- compile time environment
data Env = EnvEmpty                 -- empty environment
         | EnvPair Env Pat          -- constructed environment
         | EnvAnn  Env (Label, Pat) -- annotated environment
           deriving (Ord, Show, Eq)

data CAM = Ins Instructions -- instructions
         | Seq CAM CAM      -- sequence
         | Lab Label CAM    -- labeled sequence
         deriving (Ord, Show, Eq)





interpret :: Exp -> CAM
interpret e = Seq (codegen e EnvEmpty) (Ins STOP)

codegen :: Exp -> Env -> CAM
codegen (Var var) env = lookup var env 0
codegen (Sys (LInt n)) _ = Ins $ QUOTE (LInt n) -- s(0)
codegen (Sys (LBool b)) _ = Ins $ QUOTE (LBool b) -- s(0)
codegen (Sys (Sys1 uop e)) env = Seq (codegen e env) (Ins (PRIM1 uop))
codegen (Sys (Sys2 bop e1 e2)) env = Seq (eval e1 e2 env) (Ins (PRIM2 bop))
codegen Void _ = Ins CLEAR
codegen (Pair e1 e2) env = Seq (eval e1 e2 env) (Ins CONS)
codegen (Con tag e) env = Seq (codegen e env) (Ins $ PACK tag)
codegen (App e1 e2)  env = Seq (eval e2 e1 env) (Ins APP)

codegenR :: Exp -> Env -> CAM
codegenR e env = Seq (codegen e env) (Ins RETURN)

eval :: Exp -> Exp -> Env -> CAM
eval e1 e2 env =
  (Seq (Ins PUSH)
   (Seq (codegen e1 env)
    (Seq (Ins SWAP) (codegen e2 env))))

lookup :: Var -> Env -> Int -> CAM
lookup var EnvEmpty _ = Ins FAIL
lookup var (EnvPair env pat) n =
  (Seq (Ins (ACC n))
   (lookupPat var pat)) <?>
  (lookup var env (n + 1))
lookup var (EnvAnn env (l, pat)) n =
  (Seq (Ins (REST n))
   (Seq (Ins (CALL l))
    (lookupPat var pat))) <?>
  (lookup var env n)


lookupPat :: Var -> Pat -> CAM
lookupPat _ Empty = Ins FAIL
lookupPat x (PatVar v)
  | x == v = Ins SKIP
  | otherwise = Ins FAIL
lookupPat x (PatPair p1 p2) =
  (Seq (Ins FST) (lookupPat x p1)) <?>
  (Seq (Ins SND) (lookupPat x p2))
lookupPat x (As y p)
  | x == y = Ins SKIP
  | otherwise = lookupPat x p

(<?>) :: CAM -> CAM -> CAM
(<?>) x y
  | nofail x = x
  | nofail y = y
  | otherwise = error "Variable not found"

nofail :: CAM -> Bool
nofail (Ins FAIL) = False
nofail (Ins _)    = True
nofail (Seq _ cam2) = nofail cam2
nofail (Lab _ cam)  = nofail cam

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
3. Data constructors
   1 :: (2 :: Empty)
       |
       |  compiled to
       V
   Pair (Con "::" (Sys (LInt 1))) (Pair (Con "::" (Sys (LInt 2))) (Con Empty Void))

-}
