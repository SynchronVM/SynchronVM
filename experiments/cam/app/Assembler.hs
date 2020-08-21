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

module Assembler where

import CAM
import Data.Word
import Data.Primitive.ByteArray
import GHC.Arr



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

translate :: CAM -> ByteArray
translate cam = byteArrayFromList bytelist
  where
    bytelist = assemble cam
    len = length bytelist

assemble :: CAM -> [Word8]
assemble = undefined
