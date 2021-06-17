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

module Bytecode.StaticMemoryManagedInterpreter ( EnvContent (..)
                                               , Val (..)
                                               , evaluate) where

import CamOpt
import Peephole
import Data.Int (Int32)
import Data.List (find, delete)
import Data.Word (Word8)
import GHC.Arr
import qualified Control.Monad.State.Strict as S

import Debug.Trace

{-

The low level interpreter with an explicit heap

-}

gc :: Bool
gc = True

data PointerType
  = Frame Word8
  | Free -- pointers used by the free list;
  deriving (Ord, Show, Eq)


{- NOTE:

A pointer is plainly a number. That number will either be an index of the
heap array or it will be -1 (null). What the `PointerType` associated with that
number indicates is to which stack frame does that heap cell belong?

Frame <n> pointer - belongs to the stack frame number n
                    Eg : *2{st0}
                          ^
                         Pointer to heap cell number 2 and
                         the pointer originated at stack frame #0

Free pointer - not associated with any stack frame; part of the free list

-}

data Pointer = Pointer PointerType Int deriving (Ord, Eq)

instance Show Pointer where
  show (Pointer Free i)
    | i == -1   = "null"
    | otherwise = show i <> "f"
  show (Pointer (Frame ui) i)
    | i == -1 = "null"
    | otherwise =
        show i <> "{st" <> show ui <> "}"

data CellContent = V Val
                 | P Pointer
                 | L Label
                 | T Tag
                 deriving Eq

instance Show CellContent where
  show (V val) = show val
  show (P p) = "*"  <> show p
  show (L l) = "l:" <> show l
  show (T t) = "T:" <> show t

--type MarkBit = Bool

data HeapCell = HeapCell (CellContent, CellContent)
              deriving Eq

instance Show HeapCell where
  show (HeapCell (c1,c2)) = "<" <> show (c1,c2) <> ">"


nullPointer = Pointer Free (-1)
emptyCell   = HeapCell (P nullPointer, P nullPointer)
heapSize    = 10 --heap cells


type Heap  = Array Int HeapCell

data StackContent = SV Val | SP Pointer deriving (Show, Eq)

type Stack = [StackContent]

data EnvContent  = EV Val | EP Pointer deriving Eq

instance Show EnvContent where
  show (EV val) = show val
  show (EP ptr) = "*" <> show ptr

type Environment = EnvContent

type Index = Int

type SymbolTable = [(Label, Index)]

data Code = Code { instrs :: Array Index Instruction
                 , symbolTable :: SymbolTable
                 , prevJump    :: [Index]
                 , environment :: Environment
                 , stack       :: Stack
                 , heap        :: Heap
                 , nextFreeIdx    :: Index
                 , programCounter :: Index
                 , currentStackFramePtrs :: [Index]
                 , prevStackFramePtrs    :: [[Index]]
                 , currentFrameNo :: Word8
                 } deriving Show

newtype Evaluate a =
  Evaluate
    { runEvaluate :: S.State Code a
    }
  deriving (Functor, Applicative, Monad, S.MonadState Code)


-- Val is basically Weak Head Normal Form
data Val = VInt  Int32   -- constants s(0)
         | VFloat Float  -- constants s(0)
         | VBool Bool    -- constants s(0)
         | VEmpty        -- empty tuple
         deriving (Ord, Eq)

-- using Hinze's notation
instance Show Val where
  show (VInt i)   = show i
  show (VFloat f) = show f
  show (VBool b)  = show b
  show  VEmpty    = "()"
-- Take a sequence of stack machine instructions
-- and evaluate them to their normal form
evaluate :: CAM -> EnvContent
evaluate cam = val
  where
    code = initCode cam
    val  = S.evalState (runEvaluate eval) code

initCode :: CAM -> Code
initCode cam = Code { instrs = listArray (1, totalInstrs) caminstrs
                    , symbolTable = filteredEntries
                    , prevJump    = []
                    , environment = EV VEmpty
                    , stack       = []
                    , heap        = initHeap
                    , nextFreeIdx = 1
                    , programCounter = 1
                    , currentStackFramePtrs = []
                    , prevStackFramePtrs = []
                    , currentFrameNo = 0
                    }
  where
    instrsLabs = genInstrs cam dummyLabel
    indexedinstrsLabs = zip instrsLabs [1..]
    caminstrs = map (fst . fst) indexedinstrsLabs
    entries   = map (\((_,l),idx) -> (l,idx)) indexedinstrsLabs
    filteredEntries = filter (\(l,_) -> l /= dummyLabel) entries
    totalInstrs     = length caminstrs

initHeap :: Heap
initHeap = listArray (1, heapSize) finalHeap
  where
    nullHeap = replicate heapSize emptyCell
    tempHeap =
      zipWith (\i (HeapCell (P x, _)) -> HeapCell (P x, P (Pointer Free i)))
      (take heapSize [2..]) nullHeap
    (restcells, _) = splitAt (heapSize - 1) tempHeap
    finalHeap = restcells ++ [emptyCell]




genInstrs :: CAM -> Label -> [(Instruction, Label)]
genInstrs (Ins i) l = [(i, l)]
genInstrs (Seq c1 c2) l = genInstrs c1 l ++ genInstrs c2 dummyLabel
genInstrs (Lab l c) _   = genInstrs c l

eval :: Evaluate EnvContent
eval = do
  h <- getHeap
  currentInstr <- readCurrent
  case trace ("\n\n" <> show h <> "\n" <> show currentInstr) $ currentInstr of
    FST ->
      do { incPC; fstEnv; eval }
    SND ->
      do { incPC; sndEnv; eval }
    ACC n  ->
      do { incPC; accessnth n; eval }
    REST n ->
      do { incPC; restnth n; eval }
    PUSH ->
      do { incPC; push; eval }
    SWAP ->
      do { incPC; swap; eval }
    QUOTE (LInt i)  ->
      do { incPC; loadi i; eval }
    QUOTE (LFloat f)  ->
      do { incPC; loadf f; eval }
    QUOTE (LBool b) ->
      do { incPC; loadb b; eval }
    CLEAR ->
      do { incPC; clear; eval }
    PRIM1 uop ->
      do { incPC; unaryop uop; eval }
    PRIM2 bop ->
      do { incPC; binaryop bop; eval }
    CONS ->
      do { incPC; cons; eval }
    CUR l  ->
      do { incPC; cur l; eval }
    PACK c ->
      do { incPC; pack c; eval }
    SKIP ->
      do { incPC; eval}
    STOP -> getEnv -- base case
    APP    -> do {      app; eval }
    RETURN -> do {  retBack; eval }
    CALL l -> do { jumpTo l; eval }
    GOTO l -> do {   goto l; eval }
    GOTOFALSE l ->
      do { gotofalse l; eval; }
    SWITCH conds ->
      do { switch conds; eval; }

    -- new operands --
    MOVE ->
      do { incPC; move; eval }
    POP  ->
      do { incPC; pop; eval }
    SNOC ->
      do { incPC; snoc; eval }
    COMB l ->
      do { incPC; comb l; eval }
    GOTOIFALSE l ->
      do { gotoifalse l; eval; }
    SWITCHI conds ->
      do { switchi conds; eval; }

    i ->
      error $! "Unsupported Instruction : " <> show i

-- Symbol Table operations

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


-- Code operations

readCurrent :: Evaluate Instruction
readCurrent = do
  is <- S.gets instrs
  pc <- S.gets programCounter
  pure $! is ! pc

incPC :: Evaluate ()
incPC  = do
  pc <- S.gets programCounter
  S.modify $ \s -> s {programCounter = pc + 1}

-- jumpTo stores the jump point in the
-- prevJump field unlike goto
jumpTo :: Label -> Evaluate ()
jumpTo l = do
  pc <- S.gets programCounter
  st <- S.gets symbolTable
  pj <- S.gets prevJump
  let ix = st ~> l
  S.modify $ \s -> s { programCounter = ix
                     , prevJump = pc : pj
                     }

-- return back to the previous "jumped from" label
-- and increment the program counter by 1
retBack :: Evaluate ()
retBack = do
  freeStackAllocs
  psfp <- S.gets prevStackFramePtrs
  let h:_ = psfp -- XXX: Partial but there will always be atleast one stack
                 -- and we never return from the base caller
  S.modify $ \s -> s { currentStackFramePtrs = h
                     , prevStackFramePtrs = psfp }
  decFrameNo
  pj <- S.gets prevJump
  S.modify $ \s -> s { programCounter = head pj + 1
                     , prevJump = tail pj
                     }

getEnv :: Evaluate Environment
getEnv = S.gets environment

getStack :: Evaluate Stack
getStack = S.gets stack

getHeap :: Evaluate Heap
getHeap = S.gets heap

popAndRest :: Evaluate (StackContent, Stack)
popAndRest = do
  st <- getStack
  let (h:t) = st
  return (h, t)

fstEnv :: Evaluate ()
fstEnv = do
  e <- getEnv
  h <- getHeap
  case e of
    EP (Pointer _ pointer) ->
                  let HeapCell heapcell = h ! pointer
                      fstHeap  = fst heapcell
                   in case fstHeap of
                        V val -> S.modify $ \s -> s { environment = EV val }
                        P ptr -> S.modify $ \s -> s { environment = EP ptr }
                        L _   -> error "first cant be applied on a label"
                        T _   -> error "first cant be applied on a tag"
    EV _ -> error "EV constructor should not arise here"


sndEnv :: Evaluate ()
sndEnv = do
  e <- getEnv
  h <- getHeap
  case e of
    EP (Pointer _ pointer) ->
                  let HeapCell heapcell = h ! pointer
                      sndHeap  = snd heapcell
                   in case sndHeap of
                        V val -> S.modify $ \s -> s { environment = EV val }
                        P ptr -> S.modify $ \s -> s { environment = EP ptr }
                        L _   -> error "second cant be applied on a label"
                        T _   -> error "second cant be applied on a tag"
    EV _ -> error "EV constructor should not arise here"


accessnth :: Int -> Evaluate ()
accessnth 0 = sndEnv
accessnth n = do
  e <- getEnv
  fstEnv
  accessnth (n - 1)

restnth :: Int -> Evaluate ()
restnth 0 = pure ()
restnth n = do
  e <- getEnv
  fstEnv
  restnth (n - 1)

push :: Evaluate ()
push = do
  e  <- getEnv
  st <- getStack
  case e of
    EV val -> S.modify $ \s -> s { stack = (SV val) : st }
    EP ptr -> S.modify $ \s -> s { stack = (SP ptr) : st }

move :: Evaluate ()
move = do
  e  <- getEnv
  st <- getStack
  case e of
    EV val -> S.modify $ \s -> s { stack = (SV val) : st }
    EP ptr -> S.modify $ \s -> s { stack = (SP ptr) : st }
  S.modify $ \s -> s { environment = (EV VEmpty) }

pop :: Evaluate ()
pop = do
  (sT, sR) <- popAndRest
  S.modify $ \s -> s { stack = sR }
  case sT of
    SV val -> S.modify $ \s -> s { environment = EV val }
    SP ptr -> S.modify $ \s -> s { environment = EP ptr }


swap :: Evaluate ()
swap = do
  e      <- getEnv
  (sc, t) <- popAndRest
  case e of
    EV val -> S.modify $ \s -> s { stack = (SV val) : t }
    EP ptr -> S.modify $ \s -> s { stack = (SP ptr) : t }
  case sc of
    SV val -> S.modify $ \s -> s { environment = EV val }
    SP ptr -> S.modify $ \s -> s { environment = EP ptr }

loadi :: Int32 -> Evaluate ()
loadi i = S.modify $ \s -> s { environment = EV (VInt i) }

loadf :: Float -> Evaluate ()
loadf f = S.modify $ \s -> s { environment = EV (VFloat f) }

loadb :: Bool -> Evaluate ()
loadb b = S.modify $ \s -> s { environment = EV (VBool b) }

clear :: Evaluate ()
clear = S.modify $ \s -> s { environment = EV VEmpty }

unaryop :: UnaryOp -> Evaluate ()
unaryop uop = do
  e <- getEnv
  case uop of
    Abs -> do
      let EV (VInt i) = e -- XXX: Partial
      S.modify $ \s -> s { environment = EV $ VInt (abs i) }
    Neg -> do
      let EV (VInt i) = e
      S.modify $ \s -> s { environment = EV $ VInt (negate i) }
    NOT -> do
      let EV (VBool b) = e
      S.modify $ \s -> s { environment = EV $ VBool (not b) }
    DEC -> do
      let EV (VInt i) = e
      S.modify $ \s -> s { environment = EV $ VInt (i - 1) }

binaryop :: BinOp -> Evaluate ()
binaryop bop = do
  e      <- getEnv
  (h, t) <- popAndRest
  case bop of
    PlusI -> do
      let EV (VInt i1) = e -- XXX: Partial
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VInt (i2 + i1)
                         , stack = t
                         }
    MultiplyI -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VInt (i2 * i1)
                         , stack = t
                         }
    MinusI -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VInt (i2 - i1)
                         , stack = t
                         }
    PlusF -> do
      let EV (VFloat f1) = e -- XXX: Partial
      let SV (VFloat f2) = h
      S.modify $ \s -> s { environment = EV $ VFloat (f2 + f1)
                         , stack = t
                         }
    MultiplyF -> do
      let EV (VFloat f1) = e
      let SV (VFloat f2) = h
      S.modify $ \s -> s { environment = EV $ VFloat (f2 * f1)
                         , stack = t
                         }
    MinusF -> do
      let EV (VFloat f1) = e
      let SV (VFloat f2) = h
      S.modify $ \s -> s { environment = EV $ VFloat (f2 - f1)
                         , stack = t
                         }
    BGT -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VBool (i2 > i1)
                         , stack = t
                         }
    BLT -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VBool (i2 < i1)
                         , stack = t
                         }
    BGE -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VBool (i2 >= i1)
                         , stack = t
                         }
    BLE -> do
      let EV (VInt i1) = e
      let SV (VInt i2) = h
      S.modify $ \s -> s { environment = EV $ VBool (i2 <= i1)
                         , stack = t
                         }
    BEQ -> do
      case (e,h) of
        (EV (VInt i1), SV (VInt i2)) ->
          S.modify $ \s -> s { environment = EV $ VBool (i2 == i1)
                             , stack = t
                             }
        (EV (VFloat f1), SV (VFloat f2)) ->
          S.modify $ \s -> s { environment = EV $ VBool (f2 == f1)
                             , stack = t
                             }
        (EV (VBool b1), SV (VBool b2)) ->
          S.modify $ \s -> s { environment = EV $ VBool (b2 == b1)
                             , stack = t
                             }
        (EV VEmpty, SV VEmpty) ->
          S.modify $ \s -> s { environment = EV $ VBool True
                             , stack = t
                             }
        _ -> error "Equality not supported for other whnf types"

{-  HEAP growth and manipulation functions -}

cons :: Evaluate ()
cons = do
  e      <- getEnv
  (h, t) <- popAndRest
  ptr    <- stalloc (stackHeapTag h, envHeapTag e)
  S.modify $ \s -> s { environment = EP ptr
                     , stack = t
                     }

snoc :: Evaluate ()
snoc = do
  e      <- getEnv
  (h, t) <- popAndRest
  ptr    <- stalloc (envHeapTag e, stackHeapTag h)
  S.modify $ \s -> s { environment = EP ptr
                     , stack = t
                     }

cur :: Label -> Evaluate ()
cur l = do
  e   <- getEnv
  ptr <- stalloc (envHeapTag e, L l)
  S.modify $ \s -> s { environment = EP ptr }

comb :: Label -> Evaluate ()
comb l = do
  ptr <- stalloc (L l, L dummyLabel)
  S.modify $ \s -> s { environment = EP ptr }

pack :: Tag -> Evaluate ()
pack t = do
  e   <- getEnv
  ptr <- stalloc (T t, envHeapTag e)
  S.modify $ \s -> s { environment = EP ptr }

app :: Evaluate ()
app = do
  e      <- getEnv
  (h, t) <- popAndRest
  h_     <- getHeap
  case e of
    EP (Pointer _ ptr) -> do
      let (L label) = sndHeap -- XXX: Partial
      if label == dummyLabel
      then do
        case h of
          SV val -> S.modify $ \s -> s { environment = EV val, stack = t }
          SP ptr -> S.modify $ \s -> s { environment = EP ptr, stack = t }
        incFrameNo
        csfp <- S.gets currentStackFramePtrs
        psfp <- S.gets prevStackFramePtrs
        S.modify $ \s -> s { currentStackFramePtrs = []
                           , prevStackFramePtrs = csfp:psfp }
        let (L jl) = fstHeap
        jumpTo jl
      else do
        newPtr <- stalloc (fstHeap, stackHeapTag h)
        S.modify $ \s -> s { environment = EP newPtr
                           , stack       = t
                           }
        jumpTo label
      where
        heapcell = let HeapCell hc = h_ ! ptr
                    in hc
        fstHeap  = fst heapcell
        sndHeap  = snd heapcell
    EV _   -> error "EV constructor should not arise here"

switch :: [(Tag, Label)] -> Evaluate ()
switch conds = do
  e      <- getEnv
  (h, t) <- popAndRest
  h_     <- getHeap
  case e of
    EP (Pointer _ ptr) -> do
      let (_, label) =
            case find (\(c,_) -> c == contag || c == wildcardtag) conds of
              Just (cf, lf) -> (cf, lf)
              Nothing -> error $ "missing constructor " <> show contag
      newPtr <- stalloc (stackHeapTag h, sndHeap)
      S.modify $ \s -> s { environment = EP newPtr
                         , stack       = t
                         }
      goto label
      where
        heapcell = let HeapCell hc = h_ ! ptr
                    in hc
        fstHeap  = fst heapcell
        sndHeap  = snd heapcell
        contag   = case fstHeap of
                     T t -> t
                     _   -> error "non constructor patterns should be rewritten"
        wildcardtag = "??WILDCARD??"
    EV _   -> error "EV constructor should not arise here"


switchi :: [(Tag, Label)] -> Evaluate ()
switchi conds = do
  e      <- getEnv
  h_     <- getHeap
  case e of
    EP (Pointer _ ptr) -> do
      let (_, label) =
            case find (\(c,_) -> c == contag || c == wildcardtag) conds of
              Just (cf, lf) -> (cf, lf)
              Nothing -> error $ "missing constructor " <> show contag
      case sndHeap of
        V val -> S.modify $ \s -> s { environment = EV val }
        P ptr -> S.modify $ \s -> s { environment = EP ptr }
        _     -> error "Impossible to get tag or label here"
      goto label
      where
        heapcell = let HeapCell hc = h_ ! ptr
                    in hc
        fstHeap  = fst heapcell
        sndHeap  = snd heapcell
        contag   = case fstHeap of
                     T t -> t
                     _   -> error "non constructor patterns should be rewritten"
        wildcardtag = "??WILDCARD??"
    EV _   -> error "EV constructor should not arise here"


{-  HEAP growth and manipulation functions -}


-- goto doesn't store the previous jump
-- point into prevjump unlike jumpto
goto :: Label -> Evaluate ()
goto l = do
  pc <- S.gets programCounter
  st <- S.gets symbolTable
  let ix = st ~> l
  S.modify $ \s -> s { programCounter = ix }


gotofalse :: Label -> Evaluate ()
gotofalse l = do
  e      <- getEnv
  (h, t) <- popAndRest
  case h of
    SV val ->
      S.modify $ \s -> s { environment = EV val
                         , stack = t
                         }
    SP ptr ->
      S.modify $ \s -> s { environment = EP ptr
                         , stack = t
                         }

  case e of
    EV (VBool True)  -> incPC
    EV (VBool False) -> goto l
    _ -> do
      -- revert environment and stack back to the original state
      S.modify $ \s -> s { environment = e
                         , stack       = h : t
                         }
      error "gotofalse instuction applied to incorrect operand"


gotoifalse :: Label -> Evaluate ()
gotoifalse l = do
  e  <- getEnv
  case e of
    EV (VBool True)  -> incPC
    EV (VBool False) -> goto l
    _ -> error "gotoifalse instuction applied to incorrect operand"

dummyLabel = Label (-1)


stalloc ::(CellContent, CellContent) ->  Evaluate Pointer
stalloc hc = do
  h   <- getHeap
  i   <- S.gets nextFreeIdx
  if i == -1
  then error "Free List ran out!"
  else do
   let (HeapCell (_, sptr)) = (h ! i)
   csfp <- S.gets currentStackFramePtrs
   S.modify $ \s -> s { currentStackFramePtrs = i:csfp }
   S.modify $ \s -> s { heap = mutHeapCell h (HeapCell hc) i }
   case sptr of
      (P (Pointer Free j)) -> do -- for the last cell j = -1
          S.modify $ \s -> s { nextFreeIdx = j }
          cfn <- S.gets currentFrameNo
          pure (Pointer (Frame cfn) i)
      _ -> error "Error in stalloc: Free list cell labeled incorrectly"


palloc ::(CellContent, CellContent) ->  Evaluate Pointer
palloc hc = do
  h   <- getHeap
  i   <- S.gets nextFreeIdx
  if i == -1
  then error "Free List ran out!"
  else do
   let (HeapCell (_, sptr)) = (h ! i)
   psfp <- S.gets prevStackFramePtrs
   let prevStallocs = head psfp
   S.modify $ \s -> s { prevStackFramePtrs = (i:prevStallocs):psfp }
   S.modify $ \s -> s { heap = mutHeapCell h (HeapCell hc) i }
   case sptr of
      (P (Pointer Free j)) -> do -- for the last cell j = -1
          S.modify $ \s -> s { nextFreeIdx = j }
          cfn <- S.gets currentFrameNo
          pure (Pointer (Frame (cfn - 1)) i)
      _ -> error "Error in palloc: Free list cell labeled incorrectly"


mutHeapCell :: Array Int HeapCell -> HeapCell -> Int -> Array Int HeapCell
mutHeapCell arr hc i = arr // [(i,hc)]

freeStackAllocs :: Evaluate ()
freeStackAllocs = do
  csfp <- S.gets currentStackFramePtrs
  case csfp of
    []      -> pure ()
    (idx:_) -> do
      -- free internally mutates the csfp list
      -- so we might end up with an empty list
      -- in the next round of recursion
      free idx
      freeStackAllocs

free :: Index -> Evaluate ()
free idx = do
  h <- S.gets heap
  let (HeapCell (c1, c2)) = h ! idx

  -- The above cell goes to the free list --
  freeIdx <- S.gets nextFreeIdx
  let freeCell = HeapCell (P nullPointer, P (Pointer Free freeIdx))
  S.modify $ \s -> s { heap = mutHeapCell h freeCell idx }
  S.modify $ \s -> s { nextFreeIdx = idx }
  -- Free List addition ends --
  -- After freeing the cell remove it from the csfp list--
  csfp <- S.gets currentStackFramePtrs
  S.modify $ \s -> s { currentStackFramePtrs = delete idx csfp }
  --------------------------------------------------------

  {-
   We have already mutated the heap cell but because of the
   immutability of Haskell we are holding on to c1 and c2
   which themselves could be pointers worth tracing out and
   freeing
  -}
  freeCellContent c1
  freeCellContent c2
  where
    -- Don't free parent pointers or their constituents as well
    -- -- I hope the constituents also stay alive past this
    -- -- stack frame but hard to verify this
    -- Free pointers are already freed
    -- Only free pointers of the current and higher stack frames
    freeCellContent :: CellContent -> Evaluate ()
    freeCellContent (P (Pointer (Frame f) idx)) = do
      cfn <- S.gets currentFrameNo
      if (f < cfn)
      then pure () -- don't free parent allocs
      else free idx -- (f >= cfn)
    freeCellContent (P (Pointer Free _)) = pure ()
    freeCellContent _     = pure ()

envHeapTag :: EnvContent -> CellContent
envHeapTag (EV val) = V val
envHeapTag (EP ptr) = P ptr

stackHeapTag :: StackContent -> CellContent
stackHeapTag (SV val) = V val
stackHeapTag (SP ptr) = P ptr


incFrameNo :: Evaluate ()
incFrameNo = do
  cfn <- S.gets currentFrameNo
  S.modify $ \s -> s {currentFrameNo = cfn + 1}

decFrameNo :: Evaluate ()
decFrameNo = do
  cfn <- S.gets currentFrameNo
  S.modify $ \s -> s {currentFrameNo = cfn - 1}

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
-}








-- Examples
{-

g p = let e1 = (5,6) in
      p

main = let m = (3,2) in
       let a = (g m) in
       a
-- Rewritten to --

let v0 = \ v5 -> case v5 of {
  v1 -> let v2 = (5, 6) in v1
}
in let v3 = (3, 2) in let v4 = v0 v3 in v4

-- Rewritten to --

let v0 = \ v5 ->
           let v1 = v5 in
           let v2 = (5, 6) in
           v1 in
 let v3 = (3, 2) in
 let v4 = v0 v3  in
 v4


-}
example0 =
  Let (PatVar "v0") (Lam (PatVar "v5")
                     (Let (PatVar "v1") (Var "v5")
                      (Let (PatVar "v2") (Pair (Sys (LInt 5)) (Sys (LInt 6)))
                       (Var "v1"))))
  (Let (PatVar "v3") (Pair (Sys (LInt 3)) (Sys (LInt 2)))
   (Let (PatVar "v4") (App (Var "v0") (Var "v3"))
    (Var "v4")))


{-

g p = let e1 = (5,6) in
      let x = p in
      e1

main = let m = (3,2) in
       let a = (g m) in
       a

-- REWRITE --

let v0 = \ v6 -> case v6 of {
  v1 -> let v2 = (5, 6) in let v3 = v1 in v2
}
in let v4 = (3, 2) in let v5 = v0 v4 in v5

-- REWRITE --

let v0 = \ v6 ->
           let v1 = v6 in
           let v2 = (5,6) in
           let v3 = v1 in
           v2 in
 let v4 = (3,2) in
 let v5 = (v0, v4) in
 v5
-}


example1 =
  Let (PatVar "v0") (Lam (PatVar "v6")
                     (Let (PatVar "v1") (Var "v6")
                      (Let (PatVar "v2") (Pair (Sys (LInt 5)) (Sys (LInt 6)))
                       (Let (PatVar "v3") (Var "v1")
                        (Var "v2")))))
  (Let (PatVar "v4") (Pair (Sys (LInt 3)) (Sys (LInt 2)))
   (Let (PatVar "v5") (App (Var "v0") (Var "v4"))
    (Var "v5")))


run = evaluate . optimise . interpret
