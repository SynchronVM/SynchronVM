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

/*****************************/
/* Smaller Utility Functions */
/*****************************/

UINT heap_fst(heap_t *heap, heap_index i) {
  return heap->cells[i].data[0];
}

UINT heap_snd(heap_t *heap, heap_index i) {
  return heap->cells[i].data[1];
}

void heap_set_fst(heap_t *heap, heap_index i, UINT value, bool is_ptr) {
  heap->cells[i].data[0] = value;
  if (is_ptr) {
    heap->flags[i] |= HEAP_PTR_MASK_0;
  } else {
    heap->flags[i] &= !HEAP_PTR_MASK_0;
  }
}

void heap_set_snd(heap_t *heap, heap_index i, UINT value, bool is_ptr) {
  heap->cells[i].data[1] = value;
  if (is_ptr) {
    heap->flags[i] |= HEAP_PTR_MASK_1;
  } else {
    heap->flags[i] &= !HEAP_PTR_MASK_1;
  }
}

void heap_set_flags(heap_t *heap, heap_index i, UINT flags) {
  heap->flags[i] = flags;
}

unsigned int heap_num_free(heap_t *heap) {
  heap_index curr = heap->free_list;
  unsigned int n = 0;
  while (curr != HEAP_NULL) {
    curr = heap_snd(heap, curr);
    n ++;
  }
  return n;
}

/************************************/
/* Heap Creation and Initialization */
/************************************/

int heap_init(heap_t *heap, uint8_t *mem, unsigned int size_bytes) {

  if (!mem || !heap || size_bytes < 1024) return 0;
  
  unsigned int n_cells = size_bytes / (sizeof(heap_cell_t) + sizeof(UINT));

  // Maybe check to make sure that mem is 4bytes aligned,
  // it doesn't need to be as it is a uint8_t type. 
  heap->cells = (heap_cell_t *)mem;

  heap->flags = (UINT*)(mem + (sizeof(heap_cell_t) * n_cells));
 
  heap->bptr = (uintptr_t)heap;

  for (unsigned int i = 0; i < n_cells; i ++) {
    heap->cells[i].data[1] = i + 1;
    heap->flags[i] = HEAP_FLAGS_DEFAULT;
    heap->flags[i] = heap->flags[i] | HEAP_PTR_MASK_1;
  }
  
  heap->cells[n_cells-1].data[1] = HEAP_NULL;
  heap->free_list = 0;
  heap->size_bytes = size_bytes;
  heap->size_cells = n_cells;

  return 1;
}

/*******************/
/* Heap Allocation */
/*******************/

heap_index heap_allocate(heap_t *heap) {

  heap_index fl = heap->free_list;
  
  if (fl == HEAP_NULL) return fl;

  heap_index i = fl;
  heap->free_list = heap_snd(heap, i);
  heap_set_flags(heap, i, HEAP_FLAGS_DEFAULT);
  return i;
}

/* Dangerous function */
int heap_explicit_free(heap_t *heap, heap_index i) {

  heap_index curr = heap->free_list;
  while (curr != HEAP_NULL) {
    if (curr == i) return 0;  /* trying to explicitly free something
                                 that is already on the free_list */
    curr = heap_snd(heap, curr);
  }

  heap_set_snd(heap, i, heap->free_list, true);
  heap->free_list = i;
  return 1;
}

/**********************/
/* Garbage Collection */
/**********************/

