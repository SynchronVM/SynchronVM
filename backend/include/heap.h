/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson, Abhiroop Sarkar             				  */
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
#include <flags.h>


#define  HEAP_NULL                 -1


#define  HEAP_GC_MARK_BIT           0x80   
#define  HEAP_GC_FLAG_BIT           0x40

typedef INT heap_index; /* size of pointers are platform specific
                           so let's index into the heap as an array.
			   Trying to use -1 as "heap NULL"
			*/

typedef struct {
  UINT fst;
  UINT snd;
} heap_cell_t;


typedef struct {
  heap_cell_t  *cells;
  uintptr_t    bptr;
  unsigned int size_bytes;
  heap_index   size_cells;
  heap_index   sweep_pos;
  heap_flags_t *value_flags;     // Security and is_ptr flags
  uint8_t      *flags;           // GC flags
} heap_t;


extern unsigned int heap_num_free(heap_t *heap);

extern cam_value_t heap_fst(heap_t *heap, heap_index i);
extern cam_value_t heap_snd(heap_t *heap, heap_index i);
extern void heap_set_fst(heap_t *heap, heap_index i, cam_value_t value);
extern void heap_set_snd(heap_t *heap, heap_index i, cam_value_t value);
extern void heap_set_flags(heap_t *heap, heap_index i, heap_flags_t flags);

extern int heap_init(heap_t *heap, uint8_t *mem, unsigned int size_bytes);

extern heap_index heap_allocate(heap_t *heap);
extern void heap_mark(heap_t * heap, UINT value, value_flags_t v_flags);
#endif
