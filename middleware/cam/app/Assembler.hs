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

{-# LANGUAGE LambdaCase #-}
module Assembler where

import CAM hiding (initState)
import Control.Monad.State.Strict
import Data.Int (Int32)
import Data.Primitive.ByteArray
import Data.Word

import qualified Data.ByteString as B

{-
Bytecode format for CAM

---------------------------


FE ED CA FE    -- *Magic Number* - 4 bytes

01 01          -- *Version of bytecode* - 1 byte; *architecture* - arm, x86 - 1 byte

00 03          -- *Int Pool count* - 2 bytes - 65536 ints possible

00 0E ED 24    -- example integer `978212`, size upto 4 bytes
00 00 CE 35    -- example integer `52789`
00 01 C4 4D    -- example integer `115789`

00 0A          -- *String Pool count* - Each byte is indexed unlike Int Pool where every 4 bytes is an index.

48 65 6c 6c 6f -- Example string "Hello"
57 6f 72 6c 64 -- Example string "World"


00 00          -- *Native Pool count* - 2 bytes - index for native functions


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
CUR   <l>                      0x0AFF                      2              Assumes a max of 256 labels; we should use 3 or 4 bytes instead
PACK  <t>                      0x0BFFFF                    3              1 byte opcode; 2 bytes for tag encoded in hex, for ML/Haskell
                                                                          like langs with a lot of constructors, consider 3 or 4 bytes
SKIP                           0x0C                        1
STOP                           0x0D                        1
APP                            0x0E                        1
RETURN                         0x0F                        1
CALL <l>                       0x10FF                      2
GOTO <l>                       0x11FF                      2
GOTOFALSE <l>                  0x12FF                      2


SWITCH <n> <t> <l> ..          0x13FF...                   1 + 1 + 768    The size can have a max value of 256, 2 indices (tag (2 bytes), label(1 bytes))
        ^   ^   ^                                                         hence 3 * 256 = 768; 1 byte for size; 1 for opcode. Note this is max possible size
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



* <n> - Positive ints - 1 byte long
  <l> - Positive ints for label numbers - 1 byte long
  <b> - Boolean 1 byte long; 7 bits wasted
  <t> - Tag for a constructor - 2 bytes long
  <i> - index from int pool - max_index_size = 65536. The int itself can be upto 4 bytes long



-}

type Index = Int

type SymbolTable = [(Label, Index)]

data AssemblerState =
  AssemblerState
  { intpool :: [Word8]
  , strpool :: [Word8]
  , nativepool  :: [Word8]
  , symbolTable :: SymbolTable
  }


newtype Assembler a =
  Assembler
    { runAssembler :: State AssemblerState a
    }
  deriving (Functor, Applicative, Monad, MonadState AssemblerState)

initState :: AssemblerState
initState = undefined

translate :: CAM -> ByteArray
translate cam = byteArrayFromList bytelist
  where
    (bytelist, _) = runState (runAssembler (assemble i)) initState
    i   = instructions cam
    st  = buildST cam

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
    -- QUOTE (LInt i) -> undefined -- LOADI
    QUOTE (LBool b) ->
      gen2 loadb (bool b)
    CLEAR -> gen1 clear
    CONS  -> gen1 cons
    -- CUR l -> cur : byte (st ~> l) : restInstrs
    -- PACK t -> undefined
    SKIP -> gen1 skip
    STOP -> gen1 stop
    APP  -> gen1 app
    RETURN -> gen1 ret
    -- CALL l -> call : byte (st ~> l) : restInstrs
    -- GOTO l -> goto : byte (st ~> l) : restInstrs
    -- GOTOFALSE l -> gotofalse : byte (st ~> l) : restInstrs
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
        pure undefined

buildST :: CAM -> SymbolTable
buildST cam = filteredEntries
  where
    instrsLabs = genInstrs cam dummyLabel
    indexedinstrsLabs = zip instrsLabs [0..]
    entries = map (\((_,l),idx) -> (l,idx)) indexedinstrsLabs
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

gotofalse :: Word8
gotofalse = 18

byte :: Int -> Word8
byte n
  | n < 0 = error "unsigned byte not allowed"
  | n > 255 = error "value greater than one byte"
  | otherwise = fromIntegral n

(!!!) :: ByteArray -> Int -> Word8
(!!!) = indexByteArray

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

dummyLabel = Label (-1)

writeAssembly :: [Word8] -> IO ()
writeAssembly = B.writeFile filepath . B.pack

filepath = "/Users/abhiroopsarkar/C/Sense-VM/middleware/cam/file.sense"
