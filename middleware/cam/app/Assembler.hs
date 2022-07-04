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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Assembler ( translate
                 , genbytecode
                 , writeAssembly) where

import CamOpt hiding (initState)
import Control.Monad.State.Strict hiding (put)
import Data.Binary
import Data.Bits
import Data.Int (Int32)
import Data.Word
import GHC.Generics
import System.Directory.ProjectRoot

import qualified Data.ByteString.Lazy as B
{-
Bytecode format for CAM

---------------------------

FE ED CA FE    -- *Magic Number* - 4 bytes

FF             -- *Version of bytecode* - 1 byte;

00 03          -- *Int Pool count* - 2 bytes - 65536 ints possible

00 0E ED 24    -- example integer `978212`, size upto 4 bytes
00 00 CE 35    -- example integer `52789`
00 01 C4 4D    -- example integer `115789`

00 0A          -- *String Pool count* - Each byte is indexed unlike Int Pool where every 4 bytes is an index.

48 65 6c 6c 6f -- Example string "Hello"
57 6f 72 6c 64 -- Example string "World"


00 00          -- *Native Pool count* - 2 bytes - index for native functions
               -- each entry is 3 bytes. First two identify the function, and the last is the arity. (pc + 1 | pc + 2) forms the index,
               -- and pc + 3 is the arity
00 00 02       -- example foreign function "0", arity 2

00 00 00 FF    -- Code Length - Max size of 4 bytes
<opcode> <data>
.
.



-}

{-
Opcode format for CAM

Instructions                Hexadecimal                Size (bytes)                                   Comments
------------               ------------               -------------                                  ----------
FST                            0x00                        1
SND                            0x01                        1
ACC  <n>                       0x02FF                      2              Assumes a maximum of 256 levels of nesting
REST <n>                       0x03FF                      2              Same as above
PUSH                           0x04                        1
SWAP                           0x05                        1
LOADI <i>                      0x06FFFF                    3              Assumes the integer pool has a maximum of 65536 ints, index size is 2 byte
LOADB <b>                      0x07FF                      2              7 bits wasted as Boolean can be represented by 1 bit
CLEAR                          0x08                        1
CONS                           0x09                        1
CUR   <l>                      0x0AFFFF                    3              Assumes a max of 65536 labels; max 64KB
PACK  <t>                      0x0BFFFF                    3              1 byte opcode; 2 bytes for tag encoded in hex, for ML/Haskell
                                                                          like langs with a lot of constructors, consider 3 or 4 bytes
SKIP                           0x0C                        1
STOP                           0x0D                        1
APP                            0x0E                        1
RETURN                         0x0F                        1
CALL <l>                       0x10FFFF                    3
GOTO <l>                       0x11FFFF                    3
GOTOFALSE <l>                  0x12FFFF                    3


SWITCH <n> <t> <l> ..          0x13FF...                   1 + 1 + 1024   The size can have a max value of 256, 2 indices (tag (2 bytes), label(2 bytes))
        ^   ^   ^                                                         hence 4 * 256 = 1024; 1 byte for size; 1 for opcode. Note this is max possible size
      size  | label index
         tag for
       constructors

ABS                            0x14                        1
NEG                            0x15                        1
NOT                            0x16                        1
DEC                            0x17                        1
ADDI                           0x18                        1
MULI                           0x19                        1
MINI                           0x1A                        1
ADDF                           0x1B                        1
MULF                           0x1C                        1
MINF                           0x1D                        1
GT                             0x1E                        1
LT                             0x1F                        1
EQ                             0x20                        1
GE                             0x21                        1
LE                             0x22                        1

MOVE                           0x31                        1
POP                            0x32                        1
SNOC                           0x33                        1
COMB <l>                       0x34FFFF                    3
GOTOIFALSE <l>                 0x35FFFF                    3
SWITCHI <n> <t> <l> ...        0x36FF...                   1 + 1 + 1024
CALLRTS                        0x37FF                      1 + 1
APPF                           0x38FFFF                    1 + 2


* <n> - Positive ints - 1 byte long
  <l> - Positive ints for label numbers - 2 bytes long
  <b> - Boolean 1 byte long; 7 bits wasted
  <t> - Tag for a constructor - 2 bytes long
  <i> - index from int pool - max_index_size = 65536. The int itself can be upto 4 bytes long;
        relative index; 0 - the starting index starts from the first value of int pool

** FIXME: Currently we use the string pool for tags and not some fixed size 2 bytes tag

-}
-- NOTE: Whenever adding an instruction which uses labels remember to rectifyLabelOffset
-- NOTE: Whenever adding an instruction which uses labels remember to fix `bytecounter`
-- NOTE: Modify originalBytecodeOffset if adding any new pools etc
-- NOTE: A SenseVM program can support a maximum of 65534 tags(and not 65535) because
--       tag id 65535 is reserved for the "??WILDCARD??" tag.
-- NOTE: There is maximum of 65534 labels(lambdas, processes included) because
--       label 65535 is reserved for the label graveyard

{- NOTE: INSTRUCTIONS TO BE ADDED

ADDUI - 35
MULUI - 36
MINUI - 37
GTUI  - 38
LTUI  - 39
EQUI  - 40
GEUI  - 41
LEUI  - 42
GTF   - 43
LTF   - 44
EQF   - 45
GEF   - 46
LEF   - 47
EQB   - 48

-}




genbytecode :: CAM -> IO ()
genbytecode = writeAssembly . translate


writeAssembly :: [Word8] -> IO ()
writeAssembly bytes = do
  f <- filepath
  B.writeFile f (B.pack bytes)

type Index = Int

type TagIdx = Word16

type SymbolTable = [(Label, Index)]

type TagTable    = [(Tag, TagIdx)] -- See NOTE 1 to understand tag table

-- The standard Binary instance for String
-- prefixes the length of the String, this
-- type simply doesn't do that and encodes
-- only the characters to binary
newtype Str = Str { getStr :: String }
  deriving (Ord, Show, Eq, Generic)

instance Binary Str where
    put = mapM_ put . getStr

data AssemblerState =
  AssemblerState
  { intpool :: [[Word8]]
  , strpool :: [[Word8]]
  , nativepool  :: [[Word8]]
  , symbolTable :: SymbolTable
  , tagTable    :: TagTable
  , tagIdx      :: TagIdx
  }


newtype Assembler a =
  Assembler
    { runAssembler :: State AssemblerState a
    }
  deriving (Functor, Applicative, Monad, MonadState AssemblerState)

initState :: SymbolTable -> AssemblerState
initState st = AssemblerState [] [] [] st [] 0

originalBytecodeOffset
  = 4 -- magic number
  + 1 -- version number
  + 2 -- int pool count
  + 0 -- starts with nothing in int pool
  + 2 -- string pool size
  + 0 -- starts with nothing in string pool
  + 2 -- native pool count
  + 0 -- starts with nothing in native pool
  + 4 -- bytecode instructions size

translate :: CAM -> [Word8]
translate cam =
  let (AssemblerState ipool spool npool _ _ _) = pools
      ipoolSize = length ipool
      spoolSize = sum $ map length spool
      npoolSize = length npool
      bytelistSize = length bytelist
   in magic ++ version ++
      serializeToBytes (byte2 ipoolSize) ++ join ipool ++
      serializeToBytes (byte2 spoolSize) ++ join spool ++
      serializeToBytes (byte2 npoolSize) ++ join npool ++
      serializeToBytes (byte4 bytelistSize) ++
      rectifyLabelOffset ((ipoolSize * 4) + -- each int is 4 bytes
                          spoolSize +
                          npoolSize) bytelist
  where
    (bytelist, pools) = runState (runAssembler (assemble i)) (initState st)
    i   = instructions cam
    st  = buildST cam

magic :: [Word8]
magic = [254,237,202,254]

version :: [Word8]
version = [1]

assemble :: [Instruction] -> Assembler [Word8]
assemble [] = pure []
assemble (i : is) =
  case i of
    FST -> gen1 first
    SND -> gen1 second
    ACC  n -> gen2 acc  (byte n)
    REST n -> gen2 rest (byte n)
    PUSH -> gen1 push
    SWAP -> gen1 swap
    -- TODO: Unhandled Float
    QUOTE (LInt i32) -> do
      word8X2 <- modifyIntPool i32 -- index of int pool (16 bits)
      rs      <- assemble is
      pure $! loadi : word8X2 ++ rs
    QUOTE (LBool b) ->
      gen2 loadb (bool b)
    CLEAR -> gen1 clear
    CONS  -> gen1 cons
    CUR l -> genLabel cur l
    PACK t -> do
      word8X2 <- getTagIdBytes t
      rs      <- assemble is
      pure $! pack : word8X2 ++ rs
    SKIP -> gen1 skip
    STOP -> gen1 stop
    APP  -> gen1 app
    RETURN -> gen1 ret
    CALL l -> genLabel call l
    GOTO l -> genLabel goto l
    GOTOFALSE l -> genLabel gotofalse l
    SWITCH tagsandlabels -> do
      bytes <- mapM (\(t,l) -> genTagLabel t l) tagsandlabels
      let size = byte (length tagsandlabels)
      rs <- assemble is
      pure $! switch : size : join bytes ++ rs
    PRIM1 Abs -> gen1 absinst
    PRIM1 Neg -> gen1 neg
    PRIM1 NOT -> gen1 notinst
    PRIM1 DEC -> gen1 dec
    PRIM2 PlusI     -> gen1 addi
    PRIM2 MultiplyI -> gen1 muli
    PRIM2 MinusI    -> gen1 mini
    PRIM2 PlusF     -> gen1 addf
    PRIM2 MultiplyF -> gen1 mulf
    PRIM2 MinusF    -> gen1 minf
    PRIM2 BGT -> gen1 gt
    PRIM2 BLT -> gen1 lt
    PRIM2 BEQ -> gen1 eq
    PRIM2 BGE -> gen1 ge
    PRIM2 BLE -> gen1 le
    MOVE      -> gen1 move
    POP       -> gen1 pop
    SNOC      -> gen1 snoc
    COMB l    -> genLabel comb l
    GOTOIFALSE l -> genLabel gotoifalse l
    SWITCHI tagsandlabels -> do
      bytes <- mapM (\(t,l) -> genTagLabel t l) tagsandlabels
      let size = byte (length tagsandlabels)
      rs <- assemble is
      pure $! switchi : size : join bytes ++ rs

    CALLRTS n -> gen2 callrts n -- no need to call `byte` n already Word8
    _ -> error $! "Impossible instruction : " <> show i
    where
      gen1 word = do
        rs <- assemble is
        pure $! word : rs
      gen2 word1 word2 = do
        rs <- assemble is
        pure $! word1 : word2 : rs
      genLabel word label = do
        st <- gets symbolTable
        rs <- assemble is
        pure $! word : serializeToBytes (byte2 (st ~> label)) ++ rs
      genTagLabel tag label = do
       word8X2 <- getTagIdBytes tag
       st <- gets symbolTable
       pure $! word8X2 ++ serializeToBytes (byte2 (st ~> label))

serializeToBytes :: (Binary a) => a -> [Word8]
serializeToBytes a = B.unpack $ encode a

modifyIntPool :: Int32 -> Assembler [Word8]
modifyIntPool i32 = do
  ipool <- gets intpool
  let word8X4 = serializeToBytes i32
  modify $ \s -> s { intpool = ipool <~: word8X4 }
  pure $! serializeToBytes $ byte2 (length ipool)

modifyStringPool :: String -> Assembler [Word8]
modifyStringPool s = do
  spool <- gets strpool
  let word8Xn = serializeToBytes (Str s)
  modify $ \s -> s { strpool = spool <~: word8Xn }
  pure $! serializeToBytes $ byte2 (sum $ map length spool)

getTagIdBytes :: Tag -> Assembler [Word8]
getTagIdBytes tag
  | tag == "??WILDCARD??" = pure $ serializeToBytes (maxBound :: Word16)
  | otherwise = do
      tt <- gets tagTable
      tagId <- case tt `getIdx` tag of
                 Nothing -> do
                   tid <- gets tagIdx
                   modify $ \s -> s { tagIdx = tid + 1 }
                   modify $ \s -> s { tagTable = (tag,tid) `putIdx` tt }
                   pure tid
                 Just tid -> pure tid
      let word8X2 = serializeToBytes tagId
      pure word8X2

type NumBytes = Int
-- rectifies the offset of the labels after int pool,
-- string pool and native pool is built
rectifyLabelOffset :: NumBytes -> [Word8] -> [Word8]
rectifyLabelOffset _ [] = []
rectifyLabelOffset offset (w : ws) =
  case w of
    -- 2bytes long
    2  -> let (n :ns)  = ws -- ACC
          in  w : n : rectifyLabelOffset offset ns
    3  -> let (n : ns) = ws -- REST
          in  w : n : rectifyLabelOffset offset ns
    7  -> let (n : ns) = ws -- LOADB
          in  w : n : rectifyLabelOffset offset ns
    55 -> let (n :ns)  = ws -- CALLRTS
          in  w : n : rectifyLabelOffset offset ns

    -- 3bytes long
    6  -> let (b1 : b2 : bs) = ws -- LOADI
           in   w : b1 : b2 : rectifyLabelOffset offset bs
    11 -> let (b1 : b2 : bs) = ws -- PACK
           in   w : b1 : b2 : rectifyLabelOffset offset bs

    ---- OFFSETTING HAPPENS IN THE FOLLOWING 8 INSTRUCTIONS ---
    10 -> let (b1 : b2 : bs) = ws -- CUR
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    16 -> let (b1 : b2 : bs) = ws -- CALL
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    17 -> let (b1 : b2 : bs) = ws -- GOTO
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    18 -> let (b1 : b2 : bs) = ws -- GOTOFALSE
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    19 -> let (size : bs) = ws -- SWITCH
              sizeInt = fromIntegral size :: Int
           in if sizeInt == 0
              then w : size : rectifyLabelOffset offset bs
              else w : size : fixSwitchOffsets sizeInt offset bs

    52 -> let (b1 : b2 : bs) = ws -- COMB
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    53 -> let (b1 : b2 : bs) = ws -- GOTOFALSE
              label   = word8X2ToInt (b1,b2)
              word8X2 = serializeToBytes $ byte2 (label + offset)
           in  w : word8X2 ++ rectifyLabelOffset offset bs

    54 -> let (size : bs) = ws -- SWITCHI
              sizeInt = fromIntegral size :: Int
           in if sizeInt == 0
              then w : size : rectifyLabelOffset offset bs
              else w : size : fixSwitchOffsets sizeInt offset bs

    -- 1 byte long
    _ -> w : rectifyLabelOffset offset ws

-- Used for fixing the offset of switch and switchi
fixSwitchOffsets :: Int -> NumBytes -> [Word8] -> [Word8]
fixSwitchOffsets 0 offset bs = rectifyLabelOffset offset bs
fixSwitchOffsets n offset (t1 : t2 : l1 : l2 : bs)
  = t1 : t2 : word8X2 ++ fixSwitchOffsets (n - 1) offset bs
  where
    label   = word8X2ToInt (l1,l2)
    word8X2 = serializeToBytes $ byte2 (label + offset)
fixSwitchOffsets _ _ _ =
  error "Impossible to reach pattern according to the\
        \ switch/switchi bytecode specification"

word8X2ToInt :: (Word8, Word8) -> Int
word8X2ToInt (b1,b2) = (shift i1 8) .|. i2
  where
    i1 = fromIntegral b1 :: Int
    i2 = fromIntegral b2 :: Int

buildST :: CAM -> SymbolTable
buildST cam = filteredEntries
  where
    instrsLabs = genInstrs cam dummyLabel
    indexedinstrsLabs = bytecounter originalBytecodeOffset instrsLabs
    entries = map (\(_,l,idx) -> (l,idx)) indexedinstrsLabs
    filteredEntries = filter (\(l,_) -> l /= dummyLabel) entries

instructions :: CAM -> [Instruction]
instructions (Ins i) = [i]
instructions (Seq c1 c2) = instructions c1 ++ instructions c2
instructions (Lab _ c)   = instructions c


-- CAM is a linear sequence so when we encounter
-- Label l (Seq i1 i2).. the label `l` is for i1
-- only and we give the dummylabel to i2. If there
-- are labels associated with i2 they get labeled
-- with future labeled instructions
genInstrs :: CAM -> Label -> [(Instruction, Label)]
genInstrs (Ins i) l = [(i, l)]
genInstrs (Seq c1 c2) l = genInstrs c1 l ++ genInstrs c2 dummyLabel
genInstrs (Lab l c) _   = genInstrs c l

-- used for indexing labels which counts every byte
bytecounter :: Int -> [(Instruction, Label)] -> [(Instruction, Label, Int)]
bytecounter _ []  = []
bytecounter i ((inst, label) : xs) =
  case inst of
    -- 2 bytes long --
    ACC _  -> (inst, label, i) : bytecounter (i + 2) xs
    REST _ -> (inst, label, i) : bytecounter (i + 2) xs
    QUOTE (LBool _) -> (inst, label, i) : bytecounter (i + 2) xs
    CALLRTS _       -> (inst, label, i) : bytecounter (i + 2) xs
    -- 3 bytes long --
    -- TODO: Not handled Float
    QUOTE (LInt _)  -> (inst, label, i) : bytecounter (i + 3) xs
    CUR _           -> (inst, label, i) : bytecounter (i + 3) xs
    PACK _          -> (inst, label, i) : bytecounter (i + 3) xs
    CALL _          -> (inst, label, i) : bytecounter (i + 3) xs
    GOTO _          -> (inst, label, i) : bytecounter (i + 3) xs
    GOTOFALSE _     -> (inst, label, i) : bytecounter (i + 3) xs
    COMB _          -> (inst, label, i) : bytecounter (i + 3) xs
    GOTOIFALSE _    -> (inst, label, i) : bytecounter (i + 3) xs
    -- max 1026 bytes long --
    SWITCH tls      ->
      (inst, label, i) : bytecounter (i + 2 + 4 * length tls) xs
    SWITCHI tls     ->
      (inst, label, i) : bytecounter (i + 2 + 4 * length tls) xs
    -- all others one byte long --
    _ -> (inst, label, i) : bytecounter (i + 1) xs

first, second :: Word8
first  = 0
second = 1

acc, rest :: Word8
acc  = 2
rest = 3

push,swap :: Word8
push = 4
swap = 5

loadi, loadb :: Word8
loadi = 6
loadb = 7

bool :: Bool -> Word8
bool = \case
  True  -> 1
  False -> 0

clear, cons :: Word8
clear = 8
cons  = 9

cur :: Word8
cur = 10

pack :: Word8
pack = 11

skip, stop, app, ret, call, goto :: Word8
skip = 12
stop = 13
app  = 14
ret  = 15
call = 16
goto = 17

gotofalse, switch :: Word8
gotofalse = 18
switch = 19

absinst, neg, notinst, dec :: Word8
absinst = 20
neg     = 21
notinst = 22
dec     = 23

addi, muli, mini :: Word8
addi = 24
muli = 25
mini = 26

addf, mulf, minf :: Word8
addf = 27
mulf = 28
minf = 29

gt, lt, eq, ge, le :: Word8
gt = 30
lt = 31
eq = 32
ge = 33
le = 34

move, pop, snoc, comb :: Word8
move = 49
pop  = 50
snoc = 51
comb = 52

gotoifalse, switchi :: Word8
gotoifalse = 53
switchi    = 54

callrts :: Word8
callrts = 55

byte :: Int -> Word8
byte n
  | n < 0 = error "unsigned byte not allowed"
  | n > 255 = error "value greater than one byte"
  | otherwise = fromIntegral n

byte2 :: Int -> Word16
byte2 n
  | n < 0 = error "unsigned byte not allowed"
  | n > 65535 = error "index greater than two bytes"
  | otherwise = fromIntegral n

byte4 :: Int -> Word32
byte4 n
  | n < 0 = error "unsigned byte not allowed"
  | n > 4294967295 = error "index greater than four bytes"
  | otherwise = fromIntegral n


-- In this case we have a common error message
-- because this operation is only on a symbol table
(~>) :: SymbolTable -> Label -> Index
(~>) [] _ = error "Label missing in symbol table"
(~>) ((label,idx):st) l
  | l == label = idx
  | otherwise  = st ~> l

putST :: SymbolTable -> Label -> Index -> SymbolTable
putST st l idx = (l,idx) : st

-- snoc
(<~:) :: [a] -> a -> [a]
(<~:) xs x = xs ++ [x]

getIdx :: TagTable -> Tag -> Maybe TagIdx
getIdx [] _ = Nothing
getIdx ((tag,idx):tt) t
  | t == tag = Just idx
  | otherwise  = tt `getIdx` t

putIdx :: (Tag, TagIdx) -> TagTable -> TagTable
putIdx = (:)


emptyST :: SymbolTable
emptyST = []

dummyLabel = Label (-1)

filepath :: IO FilePath
filepath = do
  fpath <- getProjectRootCurrent
  case fpath of
    Just f -> pure $! f <> "/file.sense"
    Nothing -> error "Cannot detect project root"

-- NOTE 1
{-

A tag is encountered under two circumstances

1. In a constructor with PACK

When we get something like PACK "Nothing"

It takes the tag "Nothing" and searches the in memory
tag table to see if it is present. If it is present
it will be mapped to some Word16 value. Simply
transform the Word16 value to [Word8] and the assembler
can simply write that value.

If the value "Nothing" isn't in the in memory tag
table then use the "tagIdx" state variable and return
that value as the Word16 TagIdx. Before returning make
sure you add this value and tag pair to the tag table
and increment the "tagIdx" state variable

2. In a case expression using SWITCH/SWITCHI when pattern matching.

Either you have seen this tag before or it is new. If new
add it to TagTable or else get the TagIdx from the TagTable
and return that index. The "getTagIdBytes"" does exactly that.

3. Caveat: When encountered the tag "??WILDCARD??" the tag
value of 65535 is emitted at all times. This means across the
whole program you can have a maximum of 65534 tags(numbering
starts from 1). This restriction might be relaxed by using
Word32 or higher.

-}
