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
import GHC.Arr

{-
Bytecode format and its mapping to instructions will follow

Instructions                Hexadecimal                Size (bytes)                                   Comments
------------               ------------               -------------                                  ----------
FST                            0x00                        1
SND                            0x01                        1
ACC  <n>                       0x02FF                      2              Assumes a maximum of 256 levels of nesting
REST <n>                       0x03FF                      2              Same as above
PUSH                           0x04                        1
SWAP                           0x05                        1
LOADI <n>                      0x06FF                      2              Assumes the integer pool has a maximum of 256 integer, index size is 1 byte
LOADB <b>                      0x07FF                      2              7 bits wasted as Boolean can be represented by 1 bit
CLEAR                          0x08                        1
CONS                           0x09                        1
CUR <n>                        0x0AFF                      2              Assumes a max of 256 labels; we should use 3 or 4 bytes instead
LABEL <n>                      0x0BFF                      2              Same as above
PACK <n>                       0x0CFF                      2              Assumes max string pool size of 256, maybe should use 3 or 4 bytes
                                                                          because ML/Haskell like langs have a lot of constructors
SKIP                           0x0D                        1
STOP                           0x0E                        1
APP                            0x0F                        1
RETURN                         0x0F                        1
CALL <n>                       0x10FF                      2
GOTOFALSE <n>                  0x11FF                      2
GOTO <n>                       0x12FF                      2


SWITCH <n> <n> <n> ..          0x13FF...                   1 + 1 + 512    The size can have a max value of 256, 2 indices (tag, label) hence 2 * 256
        ^   ^   ^                                                         1 byte for size 1 for opcode. Note this is max possible size
      size  | label index
           string
         pool index

ABS                            0x14
NEG                            0x15
NOT                            0x16
DEC                            0x17
ADDI                           0x18
MULI                           0x19
MINI                           0x1A
ADDF                           0x1B
MULF                           0x1C
MINF                           0x1D
GT                             0x1E
LT                             0x1F
EQ                             0x20
GE                             0x21
LE                             0x22

-}

translate :: CAM -> Array Int Word8
translate cam = listArray (1, len) bytelist
  where
    bytelist = assemble cam
    len = length bytelist

assemble :: CAM -> [Word8]
assemble = undefined
