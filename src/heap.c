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

#include <heap.h>
#include <platform.h>

#include <stdlib.h> // later remove this include when no memory "malloced" in this file. 

/************/
/* Globals  */
/************/


heap_index free_list = HEAP_NULL;

heap_cell_t *heap = NULL;

uintptr_t heap_base_ptr = 0;

/*****************************/
/* Smaller Utility Functions */
/*****************************/

heap_index heap_ptr_to_index(uintptr_t p) {
  return (heap_index)(p - heap_base_ptr);
}

UINT heap_fst(heap_index i) {
  return heap[i].data[0];
}

UINT heap_snd(heap_index i) {
  return heap[i].data[1];
}

unsigned int heap_num_free(void) {
  heap_index curr = free_list;
  unsigned int n = 0;
  
  while (curr != HEAP_NULL) {

    curr = heap_snd(curr);
    n ++;
  }
  return n;
}


/************************************/
/* Heap Creation and Initialization */
/************************************/

/*@ requires n_cells > 0 && n_cells < N_MAX_HEAP_CELLS ; */
int heap_init(unsigned int n_cells) {

  if (heap) {
    free(heap);
  }

  heap = malloc(sizeof(heap_cell_t) * n_cells);

  if (heap) {

    heap_base_ptr = (uintptr_t)heap;

    for (unsigned int i = 0; i < n_cells; i ++) {
      heap[i].data[1] = i + 1;
      heap[i].flags = HEAP_FLAGS_DEFAULT;
      heap[i].flags = heap[i].flags | HEAP_PTR_MASK_1;
    }

    heap[n_cells-1].data[1] = HEAP_NULL;
  } else {
    return 0;
  }

  return 1;
}







/**********************/
/* Garbage Collection */
/**********************/



