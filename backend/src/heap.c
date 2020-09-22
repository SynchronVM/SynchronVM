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
#ifdef DEBUG
#include <stdio.h>
# define DEBUG_PRINT(x) printf x
#else
# define DEBUG_PRINT(x) do {} while (0)
#endif

#include <heap.h>
#include <platform.h>

/*****************************/
/* Smaller Utility Functions */
/*****************************/

/* value_flags_t heap_fst_flags(heap_t *heap, heap_index i) { */
/*   return (value_flags_t) heap->value_flags[i].fst; */
/* } */

/* value_flags_t heap_snd_flags(heap_t *heap, heap_index i) { */
/*   return (value_flags_t) heap->value_flags[i].snd; */
/* } */

/* cam_value_t heap_fst(heap_t *heap, heap_index i) { */
/*   struct cam_value_t val_f = { heap->cells[i].fst, heap->value_flags[i].fst }; */
/*   return val_f; */
/* } */

/* cam_value_t heap_snd(heap_t *heap, heap_index i) { */
/*   struct cam_value_t val_s = { heap->cells[i].snd, heap->value_flags[i].snd }; */
/*   return val_s; */
/* } */

cam_value_t get_cam_val(UINT ui, value_flags_t f){
  cam_value_t cvt = { .value = ui, .flags = f };
  return cvt;
}
cam_value_t heap_fst(heap_t *heap, heap_index i) {
  return get_cam_val(heap->cells[i].fst, heap->value_flags[i].fst);
}

cam_value_t heap_snd(heap_t *heap, heap_index i) {
  return get_cam_val(heap->cells[i].snd, heap->value_flags[i].snd);
}

void heap_set(heap_t *heap, heap_index i, cam_value_t f, cam_value_t s) {
  heap->cells[i].fst = f.value;
  heap->cells[i].snd = s.value;
  heap->value_flags[i].fst |= f.flags;
  heap->value_flags[i].snd |= s.flags;
}

void heap_set_fst(heap_t *heap, heap_index i, cam_value_t value) {
  heap->cells[i].fst = value.value;
  heap->value_flags[i].fst |= value.flags;
}

void heap_set_snd(heap_t *heap, heap_index i, cam_value_t value) {
  heap->cells[i].snd = value.value;
  heap->value_flags[i].snd |= value.flags;
}

/* unsigned int heap_num_free(heap_t *heap) { */
/*   heap_index curr = heap->free_list; */
/*   unsigned int n = 0; */
/*   while (curr != HEAP_NULL) { */
/*     curr = heap_snd(heap, curr); */
/*     n ++; */
/*   } */
/*   return n; */
/* } */

static inline void set_gc_mark(heap_t *heap, heap_index i) {
  heap->flags[i] |= HEAP_GC_MARK_BIT;
}

static inline void set_gc_flag(heap_t *heap, heap_index i) {
  heap->flags[i] |= HEAP_GC_FLAG_BIT;
}

static inline void clr_gc_mark(heap_t *heap, heap_index i) {
  heap->flags[i] &= ~HEAP_GC_MARK_BIT;
}

static inline void clr_gc_flag(heap_t *heap, heap_index i) {
  heap->flags[i] &= ~HEAP_GC_FLAG_BIT;
}

static inline int is_atomic(cam_value_t v) {
  return (v.flags & VALUE_PTR_BIT) == 0;
}

static inline int is_pointer(cam_value_t v) {
  return (v.flags & VALUE_PTR_BIT);
}

static inline int get_gc_mark(heap_t *heap, heap_index i) {
  return heap->flags[i] & HEAP_GC_MARK_BIT;
}

static inline int get_gc_flag(heap_t *heap, heap_index i) {
  return heap->flags[i] & HEAP_GC_FLAG_BIT;
}

static inline void clr_cell(heap_t *heap, heap_index i) {
  heap->flags[i] = 0;
  heap->cells[i].fst = 0;
  heap->cells[i].snd = 0;
  heap->value_flags[i].fst = 0;
  heap->value_flags[i].snd = 0;
}
/************************************/
/* Heap Creation and Initialization */
/************************************/

int heap_init(heap_t *heap, uint8_t *mem, unsigned int size_bytes) {

  if (!mem || !heap || size_bytes < 1024) return 0;

  unsigned int n_cells = size_bytes / (sizeof(heap_cell_t) + sizeof(heap_flags_t) + sizeof(uint8_t));

  // Maybe check to make sure that mem is 4bytes aligned,
  // it doesn't need to be as it is a uint8_t type.

  unsigned int value_flags_start = sizeof(heap_cell_t) * n_cells;
  unsigned int flags_start = value_flags_start + (sizeof(heap_flags_t) * n_cells);

  heap->cells = (heap_cell_t *)mem;
  heap->value_flags = (heap_flags_t*)(mem + value_flags_start);
  heap->flags = (uint8_t *)(mem + flags_start);
  heap->bptr = (uintptr_t)heap;

  for (unsigned int i = 0; i < n_cells; i ++) {
    heap->cells[i].snd = i + 1;
    heap->flags[i] = 0;
    heap->value_flags[i].snd = VALUE_PTR_BIT;
  }

  heap->cells[n_cells-1].snd = HEAP_NULL;
  heap->sweep_pos  = 0;
  heap->size_bytes = size_bytes;
  heap->size_cells = n_cells;

  return 1;
}

/*******************/
/* Heap Allocation */
/*******************/


/* Hughes Lazy sweep */

heap_index heap_allocate(heap_t *heap) {

  while (heap->sweep_pos < heap->size_cells) {

    if (get_gc_mark(heap, heap->sweep_pos)) {
      clr_gc_mark(heap, heap->sweep_pos);
      heap->sweep_pos++;
    } else {
      clr_cell(heap, heap->sweep_pos);
      return heap->sweep_pos++;
    }
  }

  heap->sweep_pos = 0;
  return HEAP_NULL; // Heap is full and a mark phase should be run
}

/**********************/
/* Garbage Collection */
/**********************/

// Deutsch-Schorr-Waite pointer reversal marking
// Todo: lots of testing and tweaking until it works.

//void heap_mark(heap_t * heap, UINT value, value_flags_t v_flags) {
void heap_mark(heap_t *heap, cam_value_t v) {
  bool done = false;
  cam_value_t curr = v;
  cam_value_t prev = get_cam_val(HEAP_NULL, VALUE_PTR_BIT);

  // Abort if value is not a pointer to a heap structure.
  if (is_atomic(curr)) return;

  // curr_val is a pointer onto the heap.
  while (!done) {

    // Follow left pointers
    while (is_pointer(curr) &&
	   (heap_index)curr.value != HEAP_NULL &&
	   !get_gc_mark(heap, curr.value)) {
      set_gc_mark(heap, curr.value);
      if (!is_atomic(curr)) {
	cam_value_t next = heap_fst(heap, curr.value);;
        cam_value_t pv = prev;
        heap_set_fst(heap, curr.value, pv);
        prev = curr;
        curr = next;
      }
    }

    while  (is_pointer(prev) &&
	    (heap_index)prev.value != HEAP_NULL &&
	    get_gc_flag(heap, prev.value)) {
      clr_gc_flag(heap, prev.value);

      cam_value_t next = heap_snd(heap, prev.value);

      heap_set_snd(heap, prev.value, curr);
      curr = prev;
      prev = next;

    }

    if (is_pointer(prev) &&
	(heap_index)prev.value == HEAP_NULL){
      done = true;

    } else {
      set_gc_flag(heap, prev.value);
      cam_value_t next = heap_fst(heap, prev.value);
      heap_set_fst(heap, prev.value, curr);

      cam_value_t hs = heap_snd(heap, prev.value);
      curr = hs;

      heap_set_snd(heap, prev.value, next);
    }
  }
}

/*******************/
/* Heap Debugging */
/*******************/

/* Inspecting the structure of the heap is sometimes */
/* useful for debugging. The second argument specifies */
/* the number of cells that should be displayed */
void heap_show(heap_t *heap, int size){
  int num_cells;
  if(size > heap->size_cells){
    num_cells = heap->size_cells;
  } else if(size < 0){
    num_cells = 0;
  } else {
    num_cells = size;
  }
  heap_index idx = 0;
  heap_cell_t curr = heap->cells[idx];
  heap_flags_t curr_flags = heap->value_flags[idx];

  for (heap_index i = 0; i < num_cells; i ++) {
    DEBUG_PRINT(("| (%u,%u) | (%u, %u) | -> ",
                curr.fst, curr.snd, curr_flags.fst, curr_flags.snd));
    idx++;
    curr = heap->cells[idx];
    curr_flags = heap->value_flags[idx];

  }

  DEBUG_PRINT(("HEAP_END\n"));
}
