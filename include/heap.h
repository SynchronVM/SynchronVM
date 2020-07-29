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

#ifndef __HEAP_H_
#define __HEAP_H_

/* Just a sketch */

#include <stdint.h>

#define WORDS_PER_CELL   7 /* Odd number here should give double word aligned memory cells ? */

/* Bit masks for flags of a memory cell */ 
const uint32_t flag_gc_mark       = 0x00000001;       /* Some GC algorithms require more bits */

const uint32_t flag_data_used_0   = 0x00040000;
const uint32_t flag_data_used_1   = 0x00080000;
const uint32_t flag_data_used_2   = 0x00100000;
const uint32_t flag_data_used_3   = 0x00200000;
const uint32_t flag_data_used_4   = 0x00400000;
const uint32_t flag_data_used_5   = 0x00800000;
const uint32_t flag_data_used_6   = 0x01000000;
const uint32_t flag_is_ptr_0      = 0x02000000;
const uint32_t flag_is_ptr_1      = 0x04000000;
const uint32_t flag_is_ptr_2      = 0x08000000;
const uint32_t flag_is_ptr_3      = 0x10000000;
const uint32_t flag_is_ptr_4      = 0x20000000;
const uint32_t flag_is_ptr_5      = 0x40000000;
const uint32_t flag_is_ptr_6      = 0x80000000;


typedef struct {
  uint32_t flags;
  uint32_t data[WORDS_PER_CELL];
} heap_cell_t;


/* Later initialize heap from a preallocaed array provided by caller */ 
extern heap_cell_t* heap_init(unsigned int n_cells);

#endif
