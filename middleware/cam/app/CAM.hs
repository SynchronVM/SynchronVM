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

import Control.Monad (replicateM)
import Data.Foldable (fold)
import Prelude hiding (lookup)
import qualified Control.Monad.State.Strict as S

type Var = String
type Tag = String
type TaggedField = (Tag, Pat)

data Exp = Var Var  -- variable
         | Sys Sys  -- Primops
         | Void     -- Empty Tuple
         | Pair Exp Exp    -- Pair
         | Con  Tag Exp    -- Constructed Value
         | App Exp Exp     -- Function application
         | Lam Pat Exp     -- Lambda Abstraction
         | If Exp Exp Exp  -- if then else
         | Let Pat Exp Exp -- Let bindings
         | Letrec [(Pat,Exp)] Exp -- letrec
         | Case Exp [(TaggedField, Exp)]
         deriving (Ord, Show, Eq)

data Sys = Sys2 BinOp Exp Exp -- BinOp
         | Sys1 UnaryOp Exp     -- UnaryOp
         | LInt Int     -- Int s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         deriving (Ord, Show, Eq)


data BinOp = Plus | Multiply  | Minus |
              BGT | BLT | BEQ | BGE   | BLE
           deriving (Ord, Eq)

instance Show BinOp where
  show Plus     = "+"
  show Multiply = "*"
  show Minus    = "-"
  show BGT       = ">"
  show BLT       = "<"
  show BEQ       = "=="
  show BGE       = ">="
  show BLE       = "<="

data UnaryOp = Abs | Neg | NOT | DEC deriving (Ord, Eq)

instance Show UnaryOp where
  show Abs = "abs"
  show Neg = "-"
  show NOT = "~"
  show DEC = "dec"

data Pat = PatVar Var
         | Empty
         | PatPair Pat Pat
         | As Var Pat      -- var `as` pat; equivalent to @ in Haskell
         deriving (Ord, Show, Eq)

data Instruction
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
   | SWITCH [(Tag, Label)] -- case expression for constructors
   | GOTO Label

   | FAIL -- a meta instruction to indicate search failure
   deriving (Ord, Show, Eq)

-- labels to identify a subroutine
newtype Label = Label Int deriving (Ord, Eq)

instance Show Label where
  show (Label i) = show i

-- compile time environment
data Env = EnvEmpty                 -- empty environment
         | EnvPair Env Pat          -- constructed environment
         | EnvAnn  Env (Pat, Label) -- annotated environment
           deriving (Ord, Show, Eq)

data CAM = Ins Instruction  -- instructions
         | Seq CAM CAM      -- sequence
         | Lab Label CAM    -- labeled sequence
         deriving (Ord, Eq)

instance Show CAM where
  show (Ins i) = show i
  show (Seq c1 c2) =
    show c1 <> ";\n" <> show c2
  show (Lab label cam) =
    "lab_"  <> show label <> " : " <> show cam

instance Semigroup CAM where
  (<>) = (<+>)

instance Monoid CAM where
  mempty = Ins SKIP

data CodegenState =
  CodegenState
  { count :: Int
  , thunks :: [CAM] -- thunks are the bytecode generated for the lambda body
  }

initState = CodegenState { count = 1, thunks = [] }

newtype Codegen a =
  Codegen
    { runCodegen :: S.State CodegenState a
    }
  deriving (Functor, Applicative, Monad, S.MonadState CodegenState)


freshLabel :: Codegen Label
freshLabel = do
  i <- S.gets count
  S.modify $ \s -> s {count = 1 + i}
  return $ Label i


interpret :: Exp -> CAM
interpret e = instrs <+> Ins STOP <+> fold thunks_
  where
    (instrs, CodegenState {thunks = thunks_} ) =
      S.runState
        (runCodegen $! codegen e EnvEmpty)
        initState

codegen :: Exp -> Env -> Codegen CAM
codegen (Var var) env = pure $! lookup var env 0
codegen (Sys (LInt n)) _  = pure $! Ins $ QUOTE (LInt n)  -- s(0)
codegen (Sys (LBool b)) _ = pure $! Ins $ QUOTE (LBool b) -- s(0)
codegen (Sys (Sys1 uop e)) env = do
  i1 <- codegen e env
  pure $! i1 <+> (Ins (PRIM1 uop))
codegen (Sys (Sys2 bop e1 e2)) env = do
  is <- codegen2 e1 e2 env
  pure $! is <+> (Ins (PRIM2 bop))
codegen Void _ = pure $! Ins CLEAR
codegen (Pair e1 e2) env = do
  is <- codegen2 e1 e2 env
  pure $! is <+> (Ins CONS)
codegen (Con tag e) env = do
  i1 <- codegen e env
  pure $! i1 <+> (Ins $ PACK tag)
codegen (App e1 e2) env = do
  is <- codegen2 e2 e1 env
  pure $! is <+> (Ins APP)
codegen (Lam pat e) env = do
  l <- freshLabel
  is <- codegenR e (EnvPair env pat)
  ts <- S.gets thunks
  S.modify $ \s -> s {thunks = (Lab l is) : ts}
  pure (Ins $ CUR l)
codegen (If e1 e2 e3) env = do
  l1 <- freshLabel
  l2 <- freshLabel
  is1 <- codegen e1 env
  is2 <- codegen e2 env
  is3 <- codegen e3 env
  pure $! Ins PUSH
      <+> is1
      <+> Ins (GOTOFALSE l1)
      <+> is2
      <+> Ins (GOTO l2)
      <+> Lab l1 is3
      <+> Lab l2 (Ins SKIP)
codegen (Case cond clauses) env = do
  labels    <- replicateM (length clauses) freshLabel
  skiplabel <- freshLabel
  cond'     <- codegen cond env
  let tagandlabel = zipWith extractTL clauses labels
  instrs <- zipWith3A (genStackClauses skiplabel) labels exps pats
  pure $! Ins PUSH
      <+> cond'
      <+> Ins (SWITCH tagandlabel)
      <+> fold instrs
      <+> Lab skiplabel (Ins SKIP)
  where
    extractTL ((t,_),_) l = (t, l)
    genStackClauses skipl label exp pat = do
      e <- codegen exp (EnvPair env pat)
      pure $! Lab label e
          <+> Ins (GOTO skipl)
    exps = map snd clauses
    pats = map (snd . fst) clauses
codegen (Let pat e1 e) env = do
  i1 <- codegen e1 env
  i  <- codegen e  (EnvPair env pat)
  pure $! Ins PUSH
      <+> i1
      <+> Ins CONS
      <+> i
codegen (Letrec recpats e) env = do
  labels  <- replicateM (length recpats) freshLabel
  let evalEnv = growEnv env (zip pats labels)
  instr  <- codegen e evalEnv
  instrs <- zipWithA codegenR exps (repeat evalEnv)
  let labeledInstrs = zipWith Lab labels instrs
  ts <- S.gets thunks
  S.modify $ \s -> s {thunks = labeledInstrs ++ ts}
  pure instr
  where
    growEnv :: Env -> [(Pat,Label)] -> Env
    growEnv finalEnv [] = finalEnv
    growEnv initEnv ((pat,label):xs) =
      growEnv (EnvAnn initEnv (pat, label)) xs

    pats = map fst recpats
    exps = map snd recpats

codegenR :: Exp -> Env -> Codegen CAM
codegenR e env = do
  i <- codegen e env
  pure $! i <+> (Ins RETURN)

codegen2 :: Exp -> Exp -> Env -> Codegen CAM
codegen2 e1 e2 env = do
  i1 <- codegen e1 env
  i2 <- codegen e2 env
  pure $! Ins PUSH
      <+> i1
      <+> Ins SWAP
      <+> i2

lookup :: Var -> Env -> Int -> CAM
lookup var EnvEmpty _ = Ins FAIL
lookup var (EnvPair env pat) n =
  (Ins (ACC n) <+> (lookupPat var pat)) <?>
  (lookup var env (n + 1))
lookup var (EnvAnn env (pat, l)) n =
  (Ins (REST n) <+> Ins (CALL l) <+> (lookupPat var pat)) <?>
  (lookup var env n)

lookupPat :: Var -> Pat -> CAM
lookupPat _ Empty = Ins FAIL
lookupPat x (PatVar v)
  | x == v = Ins SKIP
  | otherwise = Ins FAIL
lookupPat x (PatPair p1 p2) =
  ((Ins FST) <+> (lookupPat x p1)) <?>
  ((Ins SND) <+> (lookupPat x p2))
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

(<+>) :: CAM -> CAM -> CAM
(<+>) cam1 cam2 = Seq cam1 cam2

zipWithA ::   Applicative t
         =>   (a -> b -> t c)
         ->   [a]
         ->   [b]
         -> t [c]
zipWithA f xs ys = sequenceA (zipWith f xs ys)


zipWith3A ::   Applicative t
          =>   (a -> b -> c -> t d)
          ->   [a]
          ->   [b]
          ->   [c]
          -> t [d]
zipWith3A f xs ys zs = sequenceA (zipWith3 f xs ys zs)

-- NOTE:
{-
Data constructors:

   1 :: (2 :: Empty)
       |
       |  compiled to
       V
   Pair (Con "::" (Sys (LInt 1))) (Pair (Con "::" (Sys (LInt 2))) (Con Empty Void))

-}
