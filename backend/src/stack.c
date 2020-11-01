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

#include <stack.h>

int stack_init(cam_stack_t *s, uint8_t *mem, unsigned int size_bytes) {

  if (!mem || !s || size_bytes < 256) return 0;

  unsigned int num_elt = size_bytes / (sizeof(UINT) + sizeof(value_flags_t));

  // Maybe make sure that the s->data becomes 4 bytes aligned?
  s->data = (UINT*)mem;

  s->flags = (value_flags_t*)(mem + sizeof(UINT) * num_elt);

  s->sp = 0;
  s->size = num_elt;

  return 1;
}

int stack_push(cam_stack_t *s, cam_value_t cvalue) {
  if (s->sp == s->size) return 0;
  s->data[s->sp] = cvalue.value;
  s->flags[s->sp++] = cvalue.flags;
  s->size++;
  return 1;
}
/* int stack_push(cam_stack_t *s, UINT value) { */
/*   if (s->sp == s->size) return 0; */

/*   s->data[s->sp] = value; */
/*   s->flags[s->sp++] = 0; */
/*   return 1; */
/* } */
/* int stack_push_ptr(cam_stack_t *s, UINT ptr) { */
/*   if (s->sp == s->size) return 0; */

/*   s->data[s->sp] = ptr; */
/*   s->flags[s->sp++] = VALUE_PTR_BIT; */
/*   return 1; */
/* } */
int stack_pop(cam_stack_t *s, cam_register_t *r) {
  if (s->sp == 0) return 0;

  s->sp--;
  r->value = s->data[s->sp];
  r->flags = s->flags[s->sp];
  s->size--;
  return 1;
}

#ifdef DEBUG
void stack_show(cam_stack_t *stack, int size){
  int num_cells;
  if(size > (INT)stack->size){
    num_cells = stack->size;
  } else if(size < 0){
    num_cells = 0;
  } else {
    num_cells = size;
  }

  for (int i = 0; i < num_cells; i ++) {
    DEBUG_PRINT(("| %u | %u |-> ", stack->data[i], stack->flags[i]));
  }

  DEBUG_PRINT(("STACK_END\n"));
}
#endif
