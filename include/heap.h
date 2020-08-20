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

#include <stdbool.h>

#include <typedefs.h>
#include <register.h>

/* Bit masks for flags of a memory cell */
#define     HEAP_MARK_BIT_MASK        0x80000000
#define     HEAP_PTR_MASK_1           0x40000000 /* is data[1] a ptr ? */
#define     HEAP_PTR_MASK_0           0x20000000 /* is data[2] a ptr ? */

#define     HEAP_FLAGS_DEFAULT        0x00000000
#define     HEAP_NULL                 -1


typedef struct {
  UINT flags;     /* Maybe the flags portion should be split up and structured a bit */
  UINT data[2];
} heap_cell_t;

typedef INT heap_index; /* size of pointers are platform specific
                           so let's index into the heap as an array.
			   Trying to use -1 as "heap NULL"
			*/

extern unsigned int heap_num_free(void);

extern UINT heap_fst(heap_index i);
extern UINT heap_snd(heap_index i);
extern void heap_set_fst(heap_index i, UINT value, bool is_ptr);
extern void heap_set_snd(heap_index i, UINT value, bool is_ptr);
extern void heap_set_flags(heap_index i, UINT flags);

/* Later initialize heap from a preallocaed array provided by caller */
extern int heap_init(unsigned int n_cells);
extern void heap_destroy(void);

extern heap_index heap_allocate(void);
extern int heap_explicit_free(heap_index i);
#endif
