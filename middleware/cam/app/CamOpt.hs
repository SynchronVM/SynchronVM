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

module CamOpt ( Exp (..)
              , Var
              , Tag
              , Sys (..)
              , BinOp (..)
              , UnaryOp (..)
              , Label (..)
              , Instruction (..)
              , CAM (..)
              , Pat (..)
              , TaggedField
              , RTS2 (..)
              , RTS1 (..)
              , interpret
              )where

import Control.Monad (replicateM)
import Data.Foldable (fold)
import Data.Int (Int32)
import Data.Word (Word8)
import Prelude hiding (lookup)
import qualified Control.Monad.State.Strict as S
import qualified Data.Set as Set

-- XXX: When adding new primitives like RTS3 do not forget
--      to ensure the rFreeSys function is extended as well

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
         | Sys1 UnaryOp Exp   -- UnaryOp
         | LInt Int32   -- Int s(0) in cam
         | LFloat Float -- Float s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         | RTS2 RTS2 Exp Exp
         | RTS1 RTS1 Exp
         deriving (Ord, Show, Eq)

data RTS2 = SEND | SPAWNDRIVER
          deriving (Ord, Show, Eq)

data RTS1 = CHANNEL | RECV
          | SPAWN   | CHOOSE
          | SYNC
          deriving (Ord, Show, Eq)

data BinOp = PlusI | MultiplyI  | MinusI |
             PlusF | MultiplyF  | MinusF |
               BGT | BLT | BEQ  | BGE    | BLE
           deriving (Ord, Eq)

instance Show BinOp where
  show PlusI     = "+"
  show MultiplyI = "*"
  show MinusI    = "-"
  show PlusF     = "+."
  show MultiplyF = "*."
  show MinusF    = "-."
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
   | MOVE       -- move register content to stack
   | POP        -- pop the first entry of the stack and place it on the stack

     -- REGISTER OPERATIONS
   | QUOTE Sys  -- QUOTE S(0) load immediate i.e `li` from the code area to environment
   | CLEAR      -- clear environment register
   | PRIM1 UnaryOp -- PRIM s(1) then apply primop to env reg
   | PRIM2 BinOp   -- PRIM s(2) then apply primop to top of stack (arg1) and env reg(arg2)
   | CONS       -- join (stack_top, env_reg) and place it on environment register
   | CUR Label  -- build a closure with labeled exp and the environment register
   | PACK Tag   -- create tagged value with (VCon pack_val env_reg) and place on env_reg
   | SNOC       -- join (env_reg, stack_top) and place it on environment register
   | COMB Label -- place label address on the register

     -- CONTROL INSTRUCTIONS
   | SKIP   -- NoOp
   | STOP   -- halt machine
   | APP    -- function applications
   | RETURN -- return from a subroutine call
   | CALL Label
   | GOTOFALSE Label
   | SWITCH [(Tag, Label)] -- case expression for constructors
   | GOTO Label
   | GOTOIFALSE Label
   | SWITCHI [(Tag, Label)]

     -- Calling an RTS function
   | CALLRTS OperationNumber

   | FAIL -- a meta instruction to indicate search failure
   deriving (Ord, Show, Eq)

-- Used to encode the operation number of the RTS

{-

spawn     - 0
channel   - 1
sendEvt   - 2
recvEvt   - 3
sync      - 4
choose    - 5
spawnDriver - 6
-}
type OperationNumber = Word8


spawnop, channelop, sendevtop, recvevtop :: Word8
syncop, chooseop, spawndriverop :: Word8

spawnop   = 0
channelop = 1
sendevtop = 2
recvevtop = 3
syncop    = 4
chooseop  = 5
spawndriverop = 6


-- labels to identify a subroutine
newtype Label =
  Label { getLabel :: Int }
  deriving (Ord, Eq)

instance Show Label where
  show (Label i) = show i

-- See NOTE 1 to undersand the notion of marking
data EnvMark = Star
             | Normal
             deriving (Ord, Show, Eq)


-- compile time environment
data Env = EnvEmpty EnvMark                  -- empty environment
         | EnvPair  EnvMark Env Pat          -- constructed environment
         | EnvAnn   EnvMark Env (Pat, Label) -- annotated environment
           deriving (Ord, Show, Eq)

markEnv :: Env -> Env
markEnv (EnvEmpty _)     = EnvEmpty Star
markEnv (EnvPair _ e p)  = EnvPair  Star e p
markEnv (EnvAnn  _ e pl) = EnvAnn   Star e pl

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
        (runCodegen $! codegen e (EnvEmpty Normal))
        initState

codegen :: Exp -> Env -> Codegen CAM
codegen v@(Var var) env
  | rClosed v (env2Eta env) = pure $! lookupRC var env
  | otherwise = pure $! lookup var env 0
codegen (Sys (LInt n)) _  = pure $! Ins $ QUOTE (LInt n)  -- s(0)
codegen (Sys (LFloat f)) _  = pure $! Ins $ QUOTE (LFloat f)  -- s(0)
codegen (Sys (LBool b)) _ = pure $! Ins $ QUOTE (LBool b) -- s(0)
codegen (Sys (Sys1 uop e)) env = do
  i1 <- codegen e env
  pure $! i1 <+> (Ins (PRIM1 uop))
codegen (Sys (Sys2 bop e1 e2)) env = do
  is <- codegen2 e1 e2 env
  pure $! is <+> (Ins (PRIM2 bop))
codegen (Sys (RTS1 rts1 e)) env = do
  i1 <- codegen e env
  pure $! i1 <+> genrts1 rts1
codegen (Sys (RTS2 rts2 e1 e2)) env = do
  is <- codegen2 e1 e2 env
  pure $! is <+> genrts2 rts2
codegen Void _ = pure $! Ins CLEAR
codegen (Pair e1 e2) env = do
  is <- codegen2 e1 e2 env
  pure $! is <+> (Ins CONS)
codegen (Con tag e) env = do
  i1 <- codegen e env
  pure $! i1 <+> (Ins $ PACK tag)
codegen (App e1 e2) env
  | rClosed e1 (env2Eta env) = do
      i1 <- codegen e2 env
      i2 <- codegen e1 env'
      pure $! i1
          <+> (Ins MOVE)
          <+> i2
          <+> (Ins APP)
  | rClosed e2 (env2Eta env) = do
      i1 <- codegen e2 env'
      i2 <- codegen e1 env
      pure $! (Ins MOVE)
          <+> i1
          <+> (Ins SWAP)
          <+> i2
          <+> (Ins APP)
  | otherwise = do
      is <- codegen2 e2 e1 env
      pure $! is <+> (Ins APP)
  where
    env' = markEnv env
codegen expr@(Lam pat e) env
  | rClosed expr (env2Eta env) = do
      l <- freshLabel
      is <- codegenR e (EnvPair Normal env' pat)
      ts <- S.gets thunks
      S.modify $ \s -> s {thunks = (Lab l is) : ts}
      pure (Ins $ COMB l)
  | otherwise = do
      l <- freshLabel
      is <- codegenR e (EnvPair Normal env pat)
      ts <- S.gets thunks
      S.modify $ \s -> s {thunks = (Lab l is) : ts}
      pure (Ins $ CUR l)
  where
    env' = markEnv env
codegen (If e1 e2 e3) env
  | rClosed e2 (env2Eta env) && rClosed e3 (env2Eta env) = do
      l1  <- freshLabel
      l2  <- freshLabel
      is1 <- codegen e1 env
      is2 <- codegen e2 env'
      is3 <- codegen e3 env'
      pure $! is1
          <+> (Ins $ GOTOIFALSE l1)
          <+> is2
          <+> (Ins $ GOTO l2)
          <+> Lab l1 is3
          <+> Lab l2 (Ins SKIP)
  | otherwise = do
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
  where
    env' = markEnv env
codegen (Case cond clauses) env
  | allClausesClosed = do
      labels    <- replicateM (length clauses) freshLabel
      skiplabel <- freshLabel
      cond'     <- codegen cond env
      let tagandlabel = zipWith extractTL clauses labels
      instrs <- zipWith3A (genStackClauses Star skiplabel) labels exps pats
      pure $! cond'
          <+> Ins (SWITCHI tagandlabel)
          <+> fold instrs
          <+> Lab skiplabel (Ins SKIP)
  | otherwise = do
      labels    <- replicateM (length clauses) freshLabel
      skiplabel <- freshLabel
      cond'     <- codegen cond env
      let tagandlabel = zipWith extractTL clauses labels
      instrs <- zipWith3A (genStackClauses Normal skiplabel) labels exps pats
      pure $! Ins PUSH
          <+> cond'
          <+> Ins (SWITCH tagandlabel)
          <+> fold instrs
          <+> Lab skiplabel (Ins SKIP)
  where
    extractTL ((t,_),_) l = (t, l)
    genStackClauses Star skipl label exp pat = do
      e <- codegen exp (EnvPair Normal (markEnv env) pat)
      pure $! Lab label e
          <+> Ins (GOTO skipl)
    genStackClauses Normal skipl label exp pat = do
      e <- codegen exp (EnvPair Normal env pat)
      pure $! Lab label e
          <+> Ins (GOTO skipl)
    exps = map snd clauses
    pats = map (snd . fst) clauses

    allClausesClosed =
      all (== True)
      $ map (\((_,p), e) -> rClosed (Lam p e) (env2Eta env)) clauses

codegen (Let pat e1 e) env
  | rClosed (Lam pat e) (env2Eta env) = do
      i1 <- codegen e1 env
      i2 <- codegen e (EnvPair Normal env' pat)
      pure $! i1
          <+> i2
  | rClosed e1 (env2Eta env) = do
      i1 <- codegen e1 env'
      i2 <- codegen e  (EnvPair Normal env pat)
      pure $! (Ins MOVE)
          <+> i1
          <+> (Ins CONS)
          <+> i2
  | otherwise = do
      i1 <- codegen e1 env
      i  <- codegen e  (EnvPair Normal env pat)
      pure $! Ins PUSH
          <+> i1
          <+> Ins CONS
          <+> i
  where
    env' = markEnv env
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
      growEnv (EnvAnn Normal initEnv (pat, label)) xs

    pats = map fst recpats
    exps = map snd recpats

codegenR :: Exp -> Env -> Codegen CAM
codegenR e@(If e1 e2 e3) env
  | rClosed e2 (env2Eta env) && rClosed e3 (env2Eta env) = do
      l  <- freshLabel
      i1 <- codegen  e1 env
      i2 <- codegenR e2 (markEnv env)
      i3 <- codegenR e3 (markEnv env)
      pure $! i1
          <+> (Ins $ GOTOIFALSE l)
          <+> i2
          <+> (Lab l i3)
  | otherwise = do
      l  <- freshLabel
      i1 <- codegen  e1 env
      i2 <- codegenR e2 env
      i3 <- codegenR e3 env
      pure $! (Ins PUSH)
          <+> i1
          <+> (Ins $ GOTOFALSE l)
          <+> i2
          <+> (Lab l i3)

codegenR (Case cond clauses) env
  | allClausesClosed = do
      labels    <- replicateM (length clauses) freshLabel
      cond'     <- codegen cond env
      let tagandlabel = zipWith extractTL clauses labels
      instrs <- zipWith3A (genStackClauses Star) labels exps pats
      pure $! cond'
          <+> Ins (SWITCHI tagandlabel)
          <+> fold instrs
  | otherwise = do
      labels    <- replicateM (length clauses) freshLabel
      cond'     <- codegen cond env
      let tagandlabel = zipWith extractTL clauses labels
      instrs <- zipWith3A (genStackClauses Normal) labels exps pats
      pure $! Ins PUSH
          <+> cond'
          <+> Ins (SWITCH tagandlabel)
          <+> fold instrs
  where
    extractTL ((t,_),_) l = (t, l)
    genStackClauses Star label exp pat = do
      e <- codegenR exp (EnvPair Normal (markEnv env) pat)
      pure $! Lab label e
    genStackClauses Normal label exp pat = do
      e <- codegenR exp (EnvPair Normal env pat)
      pure $! Lab label e
    exps = map snd clauses
    pats = map (snd . fst) clauses

    allClausesClosed =
      all (== True)
      $ map (\((_,p), e) -> rClosed (Lam p e) (env2Eta env)) clauses


codegenR e env = do
  i <- codegen e env
  pure $! i <+> (Ins RETURN)

codegen2 :: Exp -> Exp -> Env -> Codegen CAM
codegen2 e1 e2 env
  | rClosed e2 (env2Eta env) = do
      i1 <- codegen e1 env
      i2 <- codegen e2 env'
      pure $! i1
          <+> (Ins MOVE)
          <+> i2
  | rClosed e1 (env2Eta env) = do
      i1 <- codegen e2 env
      i2 <- codegen e1 env'
      pure $! i1
          <+> (Ins MOVE)
          <+> i2
          <+> (Ins SWAP)
  | otherwise = do
      i1 <- codegen e1 env
      i2 <- codegen e2 env
      pure $! Ins PUSH
          <+> i1
          <+> Ins SWAP
          <+> i2
  where
    env' = markEnv env


lookup :: Var -> Env -> Int -> CAM
lookup var (EnvEmpty _ ) _ = Ins FAIL
lookup var (EnvPair _ env pat) n
  | isStar env = (Ins (REST n) <+> (lookupPat var pat))
  | otherwise  =
    (Ins (ACC n) <+> (lookupPat var pat)) <?>
    (lookup var env (n + 1))
lookup var (EnvAnn _ env (pat, l)) n =
  (Ins (REST n) <+> Ins (CALL l) <+> (lookupPat var pat)) <?>
  (lookup var env n)

isStar :: Env -> Bool
isStar (EnvEmpty Star)     = True
isStar (EnvPair  Star _ _) = True
isStar (EnvAnn   Star _ _) = True
isStar _                   = False

-- lookup for r-closed expressions
lookupRC :: Var -> Env -> CAM
lookupRC var (EnvEmpty _) = Ins FAIL
lookupRC var (EnvPair _ env _) = lookupRC var env
lookupRC var (EnvAnn  _ env (p, l)) =
  (Ins (CALL l) <+> lookupPat var p) <?>
  lookupRC var env

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

-- non-deterministic search;
-- not partial anymore but a FAIL meta instruction
-- in the final CAM data type indicates an incorrect IR
-- generation for which there should be a sanity check
(<?>) :: CAM -> CAM -> CAM
(<?>) x y
  | nofail x = x
  | nofail y = y
  | otherwise = Ins FAIL

nofail :: CAM -> Bool
nofail (Ins FAIL) = False
nofail (Ins _)    = True
nofail (Seq _ cam2) = nofail cam2
nofail (Lab _ cam)  = nofail cam

(<+>) :: CAM -> CAM -> CAM
(<+>) cam1 cam2 = Seq cam1 cam2

callrts = Ins . CALLRTS

genrts2 :: RTS2 -> CAM
genrts2 SEND        = callrts sendevtop
genrts2 SPAWNDRIVER = callrts spawndriverop

genrts1 :: RTS1 -> CAM
genrts1 CHANNEL = callrts channelop
genrts1 RECV    = callrts recvevtop
genrts1 SPAWN   = callrts spawnop
genrts1 CHOOSE  = callrts chooseop
genrts1 SYNC    = callrts syncop

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



------- r-free and r-closed expressions--------

type PSet = Set.Set -- synonym for powerset

data EtaEnv = EtaEmpty
            | EtaPair EtaEnv Pat
            | EtaAnn  EtaEnv (Pat, PSet Var)
            deriving (Ord, Show, Eq)


vars :: Pat -> PSet Var
vars (PatVar v) = Set.singleton v
vars Empty      = Set.empty
vars (PatPair p1 p2) = vars p1 `Set.union` vars p2
vars (As v p)        = Set.singleton v `Set.union` vars p

rClosed :: Exp -> EtaEnv -> Bool
rClosed e eta = null (rFree e eta)

rFree :: Exp -> EtaEnv -> PSet Var
rFree (Var _) EtaEmpty = Set.empty -- XXX: This case is not in Hinze's paper
rFree (Var x) (EtaPair eta p)
  | x `Set.member` (vars p) = Set.singleton x
  | otherwise               = rFree (Var x) eta
rFree (Var x) (EtaAnn eta (p, v))
  | x `Set.member` (vars p) = v
  | otherwise               = rFree (Var x) eta
rFree (Sys sys) etaenv = rFreeSys sys etaenv
rFree Void _           = Set.empty
rFree (Pair e1 e2) etaenv = rFree e1 etaenv `Set.union` rFree e2 etaenv
rFree (Con _ e) etaenv    = rFree e  etaenv
rFree (App e1 e2) etaenv  = rFree e1 etaenv `Set.union` rFree e2 etaenv
rFree (Lam p e) etaenv = rFree e (EtaPair etaenv p) `Set.difference` vars p
rFree (If e1 e2 e3) etaenv =
  rFree e1 etaenv `Set.union`
  rFree e2 etaenv `Set.union`
  rFree e3 etaenv
rFree (Case e clauses) etaenv =
  rFree e etaenv `Set.union`
  foldr Set.union Set.empty (map rFreeCond clauses)
  where
    rFreeCond ((_,p), exp) =
      rFree exp (EtaPair etaenv p) `Set.difference` vars p
rFree (Let p1 e1 e) etaenv =
  (rFree e (EtaPair etaenv p1) `Set.difference` vars p1) `Set.union`
  rFree e1 etaenv
rFree (Letrec recpats e) etaenv = rFree e eta'
  where
    eta0 = foldr (\(pat, _) eta -> EtaAnn eta (pat, Set.empty)) etaenv recpats
    eta' = fixpoint recpats eta0

    fixpoint recpats envPrevIter
      | envPrevIter == envNew = envNew -- fixpoint reached
      | otherwise = fixpoint recpats envNew
      where
        envNew = foldr (\(pat, e) eta -> EtaAnn eta (pat, rFree e envPrevIter)) etaenv recpats


rFreeSys :: Sys -> EtaEnv -> PSet Var
rFreeSys (Sys2 _ e1 e2) etaenv =
  rFree e1 etaenv `Set.union` rFree e2 etaenv
rFreeSys (Sys1 _ e) etaenv = rFree e etaenv
rFreeSys (RTS2 _ e1 e2) etaenv =
  rFree e1 etaenv `Set.union` rFree e2 etaenv
rFreeSys (RTS1 _ e) etaenv =
  rFree e etaenv
rFreeSys _ _ = Set.empty

env2Eta :: Env -> EtaEnv
env2Eta (EnvEmpty _ ) = EtaEmpty
env2Eta (EnvPair  _ env  pat) = EtaPair (env2Eta env)  pat
env2Eta (EnvAnn   _ env (pat, _)) =
  EtaAnn  (env2Eta env) (pat, vars pat)




-- NOTE:
{-
Data constructors:

   1 :: (2 :: Empty)
       |
       |  compiled to
       V
   Pair (Con "::" (Sys (LInt 1))) (Pair (Con "::" (Sys (LInt 2))) (Con Empty Void))

-}


-- NOTE 1:
{-

The EnvMark data type is used to distinguish
between ρ and ρ* as defined by Hinze. The
environment that we originally work with is ρ.
We use ρ* to `taint` a ρ environment so that the
future functions know that the lookup is to be
done in a special way.

The tainting is done using the function `markEnv`

The difference occurs in the type

<ρ*, p> represented as (EnvPair Normal (markEnv) p).
The `lookup` function has a special case for this
pattern.
-}
