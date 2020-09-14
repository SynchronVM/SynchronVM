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

#include <VMC.h>
#include <typedefs.h>
#include <register.h>

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;

  printf("TODO: Implement\n");

  /* if (!vmc_init()) { */
  /*   return 0; */
  /* } */

  /* cam_stack_t *s = &vm_containers[0].stack; */

  /* for (UINT i = 0; i < 10; i ++) { */
  /*   stack_push(s, i); */
  /* } */

  /* for (UINT i = 10; i > 0; i --) { */
  /*   cam_register_t r; */
  /*   UINT expected = i - 1; */
  /*   stack_pop(s, &r); */
  /*   if (r.value != expected) { */
  /*     printf("Stack error: expected %u, got %u\n", expected, r.value); */
  /*     return 0; */
  /*   } */
  /* } */

  /* if (s->sp != 0) { */
  /*   printf("Stack error: SP == %u\n",s->sp); */
  /*   return 0; */
  /* } */
  
  return 1;
}
