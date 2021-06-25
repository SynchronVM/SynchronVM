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

module MemoryExperiment.Interpreter2 ( EnvContent (..)
                                     , Val (..)
                                     , evaluate) where

import MemoryExperiment.CamOptMem
import Data.Int (Int32)
import Data.List (find, delete)
import Data.Word (Word8)
import GHC.Arr
import qualified Control.Monad.State.Strict as S
import qualified Data.IntMap.Strict as IMap

import Debug.Trace

{-

A low level interpreter with an explicit heap and
timely memory management

-}

type Pointer   = Int
type NumCopies = Int


data CellContent = V Val
                 | P Pointer
                 | L Label
                 | T Tag
                 deriving Eq

instance Show CellContent where
  show (V val) = show val
  show (P p)
    | p == -1 = "*null"
    | otherwise = "*" <> show p
  show (L l)
    | getLabel l == -1 = "l:dummy"
    | otherwise = "l:" <> show l
  show (T t) = "T:" <> show t

data HeapCell = HeapCell (CellContent, CellContent)
              deriving Eq

instance Show HeapCell where
  show (HeapCell (c1,c2)) = "<" <> show (c1,c2) <> ">"


nullPointer = (-1)
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
                 , ptrCopies   :: IMap.IntMap NumCopies -- key = Pointer
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
                    , ptrCopies      = IMap.empty
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
      zipWith (\i (HeapCell (P x, _)) -> HeapCell (P x, P i))
      (take heapSize [2..]) nullHeap
    (restcells, _) = splitAt (heapSize - 1) tempHeap
    finalHeap = restcells ++ [emptyCell]

-- XXX: For testing purposes
cyclicHeap :: Heap
cyclicHeap = listArray (1, heapSize) finalHeap
  where
    finalHeap = [ HeapCell (V (VInt 5), P 2)
                , HeapCell (V (VInt 3), P 1)
                , HeapCell (P nullPointer, P 4) --freeList
                , emptyCell
                ]

genInstrs :: CAM -> Label -> [(Instruction, Label)]
genInstrs (Ins i) l = [(i, l)]
genInstrs (Seq c1 c2) l = genInstrs c1 l ++ genInstrs c2 dummyLabel
genInstrs (Lab l c) _   = genInstrs c l

eval :: Evaluate EnvContent
eval = do
  h <- getHeap
  currentInstr <- readCurrent
  case trace ("\n\n" <> show h <> "\n\n" <> show currentInstr) $ currentInstr of
  -- case currentInstr of
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
    EP pointer ->
      let HeapCell (fcell, scell) = h ! pointer
      in case fcell of
           V val -> do
             S.modify $ \s -> s { environment = EV val }
             free pointer Both
           P ptr -> do
             S.modify $ \s -> s { environment = EP ptr }
             -- free pointer Second


             pcopy <- S.gets ptrCopies
             case IMap.lookup pointer pcopy of
               Nothing -> do
                 free pointer Second
               Just n -> do -- n >= 2
                 -- root is going to point to something which
                 -- has a copy; now need to update the copy count
                 -- of the child cells in the map
                 S.modify $ \s -> s { ptrCopies = IMap.insert ptr n pcopy }
                 free pointer Second


           L _   -> error "first cant be applied on a label"
           T _   -> error "first cant be applied on a tag"
    EV _ -> error "EV constructor should not arise here"
    where
      inc k = k + 1


sndEnv :: Evaluate ()
sndEnv = do
  e <- getEnv
  h <- getHeap
  case e of
    EP pointer ->
      let HeapCell (fcell, scell) = h ! pointer
      in case scell of
           V val -> do
             S.modify $ \s -> s { environment = EV val }
             free pointer Both
           P ptr -> do
             S.modify $ \s -> s { environment = EP ptr }
             -- free pointer First



             pcopy <- S.gets ptrCopies
             case IMap.lookup pointer pcopy of
               Nothing -> do
                 free pointer First
               Just n -> do -- n >= 2
                 -- root is going to point to something which
                 -- has a copy; now need to update the copy count
                 -- of the child cells in the map
                 S.modify $ \s -> s { ptrCopies = IMap.insert ptr n pcopy }
                 free pointer First


           L _   -> error "second cant be applied on a label"
           T _   -> error "second cant be applied on a tag"
    EV _ -> error "EV constructor should not arise here"
    where
      inc k = k + 1


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
    EP ptr -> do
      pcopy <- S.gets ptrCopies
      case IMap.lookup ptr pcopy of
        Nothing -> do
          S.modify $ \s -> s { ptrCopies = IMap.insert ptr 2 pcopy }
          S.modify $ \s -> s { stack = (SP ptr) : st }
        Just n -> do -- n >= 2
          S.modify $ \s -> s { ptrCopies = IMap.adjust inc ptr pcopy }
          S.modify $ \s -> s { stack = (SP ptr) : st }
  where
    inc k = k + 1

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
  -- free the environment first --
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  -- freeing ends here --
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
loadi i = do
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  S.modify $ \s -> s { environment = EV (VInt i) }

loadf :: Float -> Evaluate ()
loadf f = do
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  S.modify $ \s -> s { environment = EV (VFloat f) }

loadb :: Bool -> Evaluate ()
loadb b = do
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  S.modify $ \s -> s { environment = EV (VBool b) }

clear :: Evaluate ()
clear = do
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  S.modify $ \s -> s { environment = EV VEmpty }

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
  ptr    <- malloc (stackHeapTag h, envHeapTag e)
  S.modify $ \s -> s { environment = EP ptr
                     , stack = t
                     }

snoc :: Evaluate ()
snoc = do
  e      <- getEnv
  (h, t) <- popAndRest
  ptr    <- malloc (envHeapTag e, stackHeapTag h)
  S.modify $ \s -> s { environment = EP ptr
                     , stack = t
                     }

cur :: Label -> Evaluate ()
cur l = do
  e   <- getEnv
  ptr <- malloc (envHeapTag e, L l)
  S.modify $ \s -> s { environment = EP ptr }

comb :: Label -> Evaluate ()
comb l = do
  -- freeing logic begins --
  e <- getEnv
  case e of
    EP ptr -> free ptr Both
    _      -> pure ()
  -- freeing logic ends --

  ptr <- malloc (L l, L dummyLabel)
  S.modify $ \s -> s { environment = EP ptr }

pack :: Tag -> Evaluate ()
pack t = do
  e   <- getEnv
  ptr <- malloc (T t, envHeapTag e)
  S.modify $ \s -> s { environment = EP ptr }

app :: Evaluate ()
app = do
  e      <- getEnv
  (h, t) <- popAndRest
  h_     <- getHeap
  case e of
    EP ptr -> do
      let (L label) = sndHeap -- XXX: Partial
      if label == dummyLabel
      then do
        case h of
          SV val  -> S.modify $ \s -> s { environment = EV  val, stack = t }
          SP sptr -> S.modify $ \s -> s { environment = EP sptr, stack = t }
        let (L jl) = fstHeap
        free ptr Both -- Added freeing!!
        jumpTo jl
      else do
        -- newPtr <- malloc (fstHeap, stackHeapTag h)
        -- S.modify $ \s -> s { environment = EP newPtr
        --                    , stack       = t
        --                    }
        fstEnv
        snoc
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
    EP ptr -> do
      let (_, label) =
            case find (\(c,_) -> c == contag || c == wildcardtag) conds of
              Just (cf, lf) -> (cf, lf)
              Nothing -> error $ "missing constructor " <> show contag
      -- newPtr <- malloc (stackHeapTag h, sndHeap)
      -- S.modify $ \s -> s { environment = EP newPtr
      --                    , stack       = t
      --                    }
      sndEnv
      cons
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
    EP ptr -> do
      let (_, label) =
            case find (\(c,_) -> c == contag || c == wildcardtag) conds of
              Just (cf, lf) -> (cf, lf)
              Nothing -> error $ "missing constructor " <> show contag
      -- case sndHeap of
      --   V val  -> S.modify $ \s -> s { environment = EV val  }
      --   P ptr1 -> S.modify $ \s -> s { environment = EP ptr1 }
      --   _     -> error "Impossible to get tag or label here"

      sndEnv
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


malloc ::(CellContent, CellContent) ->  Evaluate Pointer
malloc hc = do
  h   <- getHeap
  i   <- S.gets nextFreeIdx
  if i == -1
  then error "Free List ran out!"
  else do
   let (HeapCell (_, sptr)) = (h ! i)
   S.modify $ \s -> s { heap = mutHeapCell h (HeapCell hc) i }
   case sptr of
      (P j) -> do -- for the last cell j = -1
          S.modify $ \s -> s { nextFreeIdx = j }
          pure i
      _ -> error "Error in malloc: Free list cell labeled incorrectly"



mutHeapCell :: Array Int HeapCell -> HeapCell -> Int -> Array Int HeapCell
mutHeapCell arr hc i = arr // [(i,hc)]



data FollowPath
  = None
  | First
  | Second
  | Both

-- Attempt to free the requested cell and follow pointers
free :: Index -> FollowPath-> Evaluate ()
free (-1) _ = pure ()
free idx fp = do
  pcopy <- S.gets ptrCopies
  case IMap.lookup idx pcopy of
    Nothing -> do
      h <- S.gets heap
      let (HeapCell (c1, c2)) = h ! idx

      case c1 of
        (P (-1)) -> pure () -- Landed on the free list. STOP Recursion!
        _ -> do
          -- The above cell goes to the free list --
          freeIdx <- S.gets nextFreeIdx
          let freeCell = HeapCell (P nullPointer, P freeIdx)
          S.modify $ \s -> s { heap = mutHeapCell h freeCell idx }
          S.modify $ \s -> s { nextFreeIdx = idx }
          -- Free List addition ends --

          {-
           We have already mutated the heap cell but because of the
           immutability of Haskell we are holding on to c1 and c2 which
           themselves could be pointers worth tracing out and freeing
          -}

          case fp of
            None   -> pure ()
            First  -> freeCellContent c1
            Second -> freeCellContent c2
            Both   -> do
              freeCellContent c1
              freeCellContent c2

    Just 2  ->
      {- We remove the cell from the map and
         it can be freed the next time -}
      S.modify $ \s -> s { ptrCopies = IMap.delete idx pcopy }
    Just n  ->
      {- n > 2 we simply decrement the count -}
      S.modify $ \s -> s { ptrCopies = IMap.adjust dec idx pcopy }
  where
    dec k = k - 1

    -- We want to trace out pointers and free them
    freeCellContent :: CellContent -> Evaluate ()
    freeCellContent (P (-1)) = pure ()
    freeCellContent (P ptr) = free ptr Both
    freeCellContent _     = pure ()


envHeapTag :: EnvContent -> CellContent
envHeapTag (EV val) = V val
envHeapTag (EP ptr) = P ptr

stackHeapTag :: StackContent -> CellContent
stackHeapTag (SV val) = V val
stackHeapTag (SP ptr) = P ptr


-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register
-}
