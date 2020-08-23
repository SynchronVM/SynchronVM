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

#ifndef __VMC_H_
#define __VMC_H_

#include <vm-conf.h>

#include <stdint.h>

#define VMC_CONTAINER_1 0
#define VMC_CONTAINER_2 1

typedef struct {
  uint8_t       *heap_memory;
  const uint8_t *code_memory;
} vmc_t;


#if VMC_NUM_CONTAINERS >= 1 && VMC_NUM_CONTAINERS <= 10 

vmc_t vm_containers[VMC_NUM_CONTAINERS];

#else
/* This is just an experiment and if we end up building on it, the 
   range of numbers can be extended */
#error "VMC_NUM_CONTAINERS must be set to an integer value from 1 to 2"
#endif 


#if VMC_NUM_CONTAINERS >= 1
uint8_t vmc_container_1_heap[VMC_CONTAINER_1_HEAP_SIZE_BYTES];

const uint8_t vmc_container_1_code[] = {
  #include VMC_CONTAINER_1_BYTECODE_FILE
  ,0
};
				      
#endif 

#if VMC_NUM_CONTAINERS >= 2
unsigned char vmc_container_2_heap[VMC_CONTAINER_2_HEAP_SIZE_BYTES];

const uint8_t vmc_container_2_code[] = {
  #include VMC_CONTAINER_2_BYTECODE_FILE
  ,0
};

#endif 



/**********************/
/* External Interface */
/**********************/


extern int vmc_init(void);


#endif
