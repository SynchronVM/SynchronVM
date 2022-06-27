-- MIT License

-- Copyright (c) 2021 Abhiroop Sarkar, Joel Svensson

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
optimise cam = traceShowId cam'
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
    o <- applyThreeOPRule
    incPCBy o
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
          let (is_, offset) = oneOPRule is i1_
          replaceNInstrs 1 Nothing is_
          pure offset
      
      Labeled l (Ins il_) -> do
          let (is_, offset) = oneOPRule is il_
          replaceNInstrs 1 (Just l) is_
          pure 1 -- a labeled instruction becomes a new labeled instruction, so
                 -- we should just progress the PC by 1 instruction

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
          replaceNInstrs 2 Nothing is_
          pure offset
      (Labeled l (Ins i1_), Plain (Ins i2_)) -> do
          let (is_, offset) = twoOPRule i1_ i2_
          replaceNInstrs 2 (Just l) is_
          pure offset
      _ -> pure 0 -- maybe the OneOPRule applies so don't increment PC

  else pure 0 -- if there is only one instruction and applyOneOPRule applies

{-
The 3 op rule was added to deal with certain scenarios
which arise in the `else` branch of an if-then-else construct.
We end up with bytecode sequences like:

CALL L1 -- L1 will be the beginning of the if statement
L2 : SKIP
RETURN

We would like to optimise this to

CALL L1
L2 : RETURN

and then

GOTO L1

But because of the labeled SKIP instuction this is quite complicated in
the current machinery. The 3-OP rule studies this specific sequence
and transforms this to:

GOTO L1
L2 : SKIP
RETURN

The last 2 bytecodes *might* be dead code. But we need a dedicated dead-code
elimination pass which studies the whole bytecode and figures this out.

-}
applyThreeOPRule :: Optimise Offset
applyThreeOPRule = do
  is <- getInstrs
  pc <- getPC
  b  <- atleastNInstrs 3
  if b
  then do
    let i1 = is !! pc
    let i2 = is !! (pc + 1) -- the if check makes sure that PC doesnt go out of bound
    let i3 = is !! (pc + 2) -- the if check makes sure that PC doesnt go out of bound
    case (i1, i2, i3) of
      (Plain (Ins (CALL lab)), Labeled l (Ins SKIP), Plain (Ins RETURN)) -> do
          replaceNInstrs 1 Nothing [GOTO lab]
          pure 1
      _ -> pure 0 -- maybe the twoOp/OneOPRule applies so don't increment PC

  else pure 0 -- apply twoOpRule, oneOpRule etc




incPCBy :: Offset -> Optimise ()
incPCBy offset = do
  pc <- getPC
  if (offset < 0 && pc <= 0) then return ()
    else  S.modify $ \s -> s {programcounter = pc + offset}

{- | Replace @n@ instructions from the current @pc@ with instructions @is_@. If the label @l@ is
a @Just@, the interleaved statements are @Labeled l SKIP@ if @is_@ is empty, and @Labeled l (head i) : is@ if
  it @is_@ is not empty.
  
  Result is
  
  @
  concat [ take pc instructions
         , new interleaved instructions
         , drop (pc + n) instructions
         ]
  @-}
replaceNInstrs :: Int -> Maybe Label -> [Instruction] -> Optimise ()
replaceNInstrs n l is_ = do
  is <- getInstrs
  pc <- getPC
  let replacement' = map flatcaminst is_
  let replacement  = case l of
                       Just l' -> labelheadcaminst l' replacement'
                       Nothing -> replacement'
  let instrs_ = take pc is
             ++ replacement
             ++ drop (pc + n) is
  S.modify $ \s -> s {instrs = instrs_}
  where
    flatcaminst :: Instruction -> FlatCAM
    flatcaminst i = Plain (Ins i)


    labelheadcaminst :: Label -> [FlatCAM] -> [FlatCAM]
    labelheadcaminst l [] = [Labeled l (Ins SKIP)]
    labelheadcaminst l ((Plain i):is) = (Labeled l i):is
    labelheadcaminst _ is = is  -- Even empty replacement seq is legal

    -- There are a couple of problems here that we need to protect ourselves against.
    --
    -- In a two instr sequence both could be labeled.
    -- so if a two instr sequence is transformed into a one instr sequence
    -- then it should have two labels on that instruction. Or rather, usages
    -- of the first of these labels should be replaced with usages of the second. 
    -- Here,  applyTwoOpRule will not apply any two instr seq optimizations
    -- if both instrs are labeled, hopefully avoiding that issue.
    --
    -- Another thing is when a two instruction sequence is translated to a
    -- 0 instruction sequence. Then the label should be moved to the next
    -- instruction in the sequence.
    -- Here we protect against that by inserting a labeled SKIP in this case.
    --

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

setPC :: Int -> Optimise ()
setPC newpc = do
  (Code i pc) <- S.get
  S.put (Code i newpc)

getInstrs :: Optimise [FlatCAM]
getInstrs = do
  is <- S.gets instrs
  pure is

type Offset = Int

lookupFun :: Label -> [FlatCAM] -> Maybe [Instruction]
lookupFun l [] = Nothing
lookupFun l func@((Labeled l1 x):xs) | l == l1 =
                                       grabFunc func
                                     | otherwise = lookupFun l xs
  where
    grabFunc :: [FlatCAM] -> Maybe [Instruction]
    grabFunc ((Labeled l1 (Ins i)):(Plain (Ins RETURN)):xs) = Just [i] 
    grabFunc _ = Nothing
lookupFun l (x:xs) = lookupFun l xs


lookupLabGoto :: Label -> [FlatCAM] -> Maybe [Instruction]
lookupLabGoto l [] = Nothing
lookupLabGoto l func@((Labeled l1 x):xs) | l == l1 =
                                       grabFunc func
                                     | otherwise = lookupLabGoto l xs
  where
    -- In the goto case we need to retain the return. 
    grabFunc :: [FlatCAM] -> Maybe [Instruction]
    grabFunc ((Labeled l1 (Ins i)):(Plain (Ins RETURN)):xs) = Just [i, RETURN]
    grabFunc _ = Nothing
lookupLabGoto l (x:xs) = lookupLabGoto l xs



oneOPRule :: [FlatCAM] -> Instruction -> ([Instruction], Offset)
oneOPRule _ SKIP     = ([], -1)
oneOPRule _ (REST 0) = ([], -1)
oneOPRule _ (REST 1) = ([FST], -1)
oneOPRule _ (ACC  0) = ([SND], -1)
oneOPRule c (CALL l) = case is of
                         (Just is') -> (is', -1)
                         Nothing    -> ([CALL l], 1)
  where is = lookupFun l c
oneOPRule c (GOTO l) = case is of
                         (Just is') -> (is', -1)
                         Nothing    -> ([GOTO l], 1)
  where is = lookupLabGoto l c
oneOPRule _ i        = ([i]  ,  1)

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
