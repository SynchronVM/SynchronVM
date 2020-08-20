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
  /* 3 * 4 bytes per cell = 12 bytes */
  uintptr_t ix = (p - heap_base_ptr) / 12;
  return (heap_index)ix;
}

UINT heap_fst(heap_index i) {
  return heap[i].data[0];
}

UINT heap_snd(heap_index i) {
  return heap[i].data[1];
}

void heap_set_fst(heap_index i, UINT value, bool is_ptr) {
  heap[i].data[0] = value;
  if (is_ptr) {
    heap[i].flags |= HEAP_PTR_MASK_0;
  } else {
    heap[i].flags &= !HEAP_PTR_MASK_0;
  }
}

void heap_set_snd(heap_index i, UINT value, bool is_ptr) {
  heap[i].data[1] = value;
  if (is_ptr) {
    heap[i].flags |= HEAP_PTR_MASK_1;
  } else {
    heap[i].flags &= !HEAP_PTR_MASK_1;
  }
}

void heap_set_flags(heap_index i, UINT flags) {
  heap[i].flags = flags;
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
    free_list = 0;
  } else {
    return 0;
  }
  return 1;
}


void heap_destroy(void) {
  if (heap)
    free(heap);

  free_list = HEAP_NULL;
}

/*******************/
/* Heap Allocation */
/*******************/

heap_index heap_allocate(void) {

  if (free_list == HEAP_NULL) return free_list;

  heap_index i = free_list;
  free_list = heap_snd(i);
  heap_set_flags(i, HEAP_FLAGS_DEFAULT);
  return i;
}

/* Dangerous function */
int heap_explicit_free(heap_index i) {

  heap_index curr = free_list;
  while (curr != HEAP_NULL) {
    if (curr == i) return 0;  /* trying to explicitly free something
                                 that is already on the free_list */
    curr = heap_snd(curr);
  }

  heap_set_snd(i, free_list, true);
  free_list = i;
  return 1;
}

/**********************/
/* Garbage Collection */
/**********************/

