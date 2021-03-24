/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson             				  */
/* 										  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy	  */
/* of this software and associated documentation files (the "Software"), to deal  */
/* in the Software without restriction, including without limitation the rights	  */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell	  */
/* copies of the Software, and to permit persons to whom the Software is	  */
/* furnished to do so, subject to the following conditions:			  */
/* 										  */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software.				  */
/* 										  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR	  */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,	  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE	  */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER	  */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  */
/* SOFTWARE.									  */
/**********************************************************************************/

#include <CAM.h>

const uint8_t bci_fst           = 1;
const uint8_t bci_snd           = 2;
const uint8_t bci_acc           = 3;
const uint8_t bci_rest          = 4;
const uint8_t bci_push          = 5; // May need more push operations for different sizes ?
const uint8_t bci_swap          = 6;

const uint8_t bci_li8           = 7; // These are replacements for "Quote" in Ralf Hinze's paper
const uint8_t bci_lu8           = 8;
const uint8_t bci_li32          = 9;
const uint8_t bci_lu32          = 10;
const uint8_t bci_lf32          = 11;

const uint8_t bci_clear         = 12;

const uint8_t bci_addi8         = 13;
const uint8_t bci_addu8         = 14;
const uint8_t bci_addi32        = 15;
const uint8_t bci_addu32        = 16;
const uint8_t bci_addf32        = 17;

const uint8_t bci_subi8         = 18;
const uint8_t bci_subu8         = 19;
const uint8_t bci_subi32        = 20;
const uint8_t bci_subu32        = 21;
const uint8_t bci_subf32        = 22;

const uint8_t bci_muli8         = 23;
const uint8_t bci_mulu8         = 24;
const uint8_t bci_muli32        = 25;
const uint8_t bci_mulu32        = 26;
const uint8_t bci_mulf32        = 27;

/* Add more primitive operations */

const uint8_t bci_tup           = 28; // feels like a better name for "cons" from the paper
const uint8_t bci_cur           = 29; // make a ClosURe
const uint8_t bci_pack          = 30;
const uint8_t bci_skip          = 31;
const uint8_t bci_stop          = 32;
const uint8_t bci_app           = 33;
const uint8_t bci_ret           = 34;
const uint8_t bci_call          = 35;
const uint8_t bci_jmpf          = 36; // jump if false
const uint8_t bci_switch        = 37;
const uint8_t bci_goto          = 38;
