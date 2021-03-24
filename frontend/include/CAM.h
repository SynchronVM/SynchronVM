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

#ifndef __CAM_H_
#define __CAM_H_

#include <stdint.h>

extern const uint8_t bci_fst;
extern const uint8_t bci_snd;
extern const uint8_t bci_acc;
extern const uint8_t bci_rest;
extern const uint8_t bci_push; // May need more push operations for different sizes ?
extern const uint8_t bci_swap;

extern const uint8_t bci_li8; // These are replacements for "Quote" in Ralf Hinze's paper
extern const uint8_t bci_lu8;
extern const uint8_t bci_li32;
extern const uint8_t bci_lu32;
extern const uint8_t bci_lf32;

extern const uint8_t bci_clear;

extern const uint8_t bci_addi8;
extern const uint8_t bci_addu8;
extern const uint8_t bci_addi3;
extern const uint8_t bci_addu32;
extern const uint8_t bci_addf32;

extern const uint8_t bci_subi8;
extern const uint8_t bci_subu8;
extern const uint8_t bci_subi32;
extern const uint8_t bci_subu32;
extern const uint8_t bci_subf32;

extern const uint8_t bci_muli8;
extern const uint8_t bci_mulu8;
extern const uint8_t bci_muli32;
extern const uint8_t bci_mulu32;
extern const uint8_t bci_mulf32;

/* Add more primitive operations */

extern const uint8_t bci_tup; // feels like a better name for "cons" from the paper
extern const uint8_t bci_cur; // make a ClosURe
extern const uint8_t bci_pack;
extern const uint8_t bci_skip;
extern const uint8_t bci_stop;
extern const uint8_t bci_app;
extern const uint8_t bci_ret;
extern const uint8_t bci_call;
extern const uint8_t bci_jmpf; // jump if false
extern const uint8_t bci_switch;
extern const uint8_t bci_goto;

/* cam_context_t

   Info: Representation of the state of a running CAM process

   reg: value register
   pc: index into code area where currently executing

   stack:
   code area: pointer to the bytecode
*/
typedef struct {

  uint32_t reg;
  uint32_t pc;

  /* stack */
  /* code area */

} cam_context_t;

#endif
