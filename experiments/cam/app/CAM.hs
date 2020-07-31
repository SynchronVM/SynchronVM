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

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CAM where

import Prelude hiding (lookup)
import qualified Control.Monad.State.Strict as S

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


data BinOp = Plus | Multiply | Minus deriving (Ord, Eq)

instance Show BinOp where
  show Plus     = "+"
  show Multiply = "*"
  show Minus    = "-"

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

-- compile time environment
data Env = EnvEmpty                 -- empty environment
         | EnvPair Env Pat          -- constructed environment
         | EnvAnn  Env (Label, Pat) -- annotated environment
           deriving (Ord, Show, Eq)

data CAM = Ins Instructions -- instructions
         | Seq CAM CAM      -- sequence
         | Lab Label CAM    -- labeled sequence
         deriving (Ord, Eq)

instance Show CAM where
  show (Ins i) = show i
  show (Seq c1 c2) =
    show c1 <> ";\n" <> show c2
  show (Lab label cam) =
    show label <> " : " <> show cam <> "\n"

data CodegenState =
  CodegenState
  { count :: Int }

initState = CodegenState {count = 0 }

newtype Codegen a =
  Codegen
    { runCodegen :: S.State CodegenState a
    }
  deriving (Functor, Applicative, Monad, S.MonadState CodegenState)

freshLabel :: Codegen Label
freshLabel = do
  i <- S.gets count
  S.modify $ \s -> s {count = 1 + i}
  return $ "label_" <> (show i)


interpret :: Exp -> CAM
interpret e = Seq instrs (Ins STOP)
  where
    instrs = S.evalState
               (runCodegen $! codegen e EnvEmpty)
               initState

codegen :: Exp -> Env -> Codegen CAM
codegen (Var var) env = pure $! lookup var env 0
codegen (Sys (LInt n)) _ = pure $! Ins $ QUOTE (LInt n) -- s(0)
codegen (Sys (LBool b)) _ = pure $! Ins $ QUOTE (LBool b) -- s(0)
codegen (Sys (Sys1 uop e)) env = do
  i1 <- codegen e env
  pure $! Seq i1 (Ins (PRIM1 uop))
codegen (Sys (Sys2 bop e1 e2)) env = do
  is <- codegen2 e1 e2 env
  pure $! Seq is (Ins (PRIM2 bop))
codegen Void _ = pure $! Ins CLEAR
codegen (Pair e1 e2) env = do
  is <- codegen2 e1 e2 env
  pure $! Seq is (Ins CONS)
codegen (Con tag e) env = do
  i1 <- codegen e env
  pure $! Seq i1  (Ins $ PACK tag)
codegen (App e1 e2) env = do
  is <- codegen2 e2 e1 env
  pure $! Seq is (Ins APP)
codegen (Lam pat e) env = do
  l <- freshLabel
  is <- codegenR e (EnvPair env pat)
  pure $! Seq (Ins $ CUR l) is

codegenR :: Exp -> Env -> Codegen CAM
codegenR e env = do
  i <- codegen e env
  pure $! Seq i (Ins RETURN)

codegen2 :: Exp -> Exp -> Env -> Codegen CAM
codegen2 e1 e2 env = do
  i1 <- codegen e1 env
  i2 <- codegen e2 env
  pure $! (Seq (Ins PUSH)
           (Seq i1
            (Seq (Ins SWAP) i2)))

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

-- non-deterministic search; partial function
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


type Stack = [Val]

type EnvReg = Val -- the environment register

-- Take a sequence of stack machine instructions
-- and evaluate them to their normal form
eval :: CAM -> Stack -> EnvReg -> Val
eval cam stack envreg = undefined


example = Lam (PatVar "x") (Sys $ Sys2 Plus (Sys $ LInt 1) (Var "x"))
