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

#include <VMC.h>

#include <vm-conf.h>
#include <VMC.h>

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
  (void)argc;
  (void)argv;

  vmc_t vm_containers[1];

  
  if (vmc_init(vm_containers, 1) < 1) {
    return 0;
  }

  heap_t *hptr = &vm_containers[VMC_CONTAINER_1].heap;

  printf("Heap size bytes: %u\n", hptr->size_bytes);
  printf("Heap size cells: %u\n", hptr->size_cells);
  //printf("Heap free: %u\n", heap_num_free(hptr));

  //if (hptr->size_cells != heap_num_free(hptr)) {
  //  return 0;
  //}

  if ((hptr->bptr & 0x3) != 0) {
    printf("Heap is not 4 byte aligned\n");
    return 0;
  } 

  return 1;
}
