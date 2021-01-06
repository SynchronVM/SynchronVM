/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar, Joel Svensson             		  */
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
#include <typedefs.h>
#include <register.h>
#include <heap.h>
#include <channel.h>
#include <Context.h>

#include <stdint.h>

#define VMC_CONTAINER_1 0
#define VMC_CONTAINER_2 1

#define VMC_MAX_CONTEXTS 16

#define MAX_CHANNELS 100 // This number should be configurable or statically analyzable from the code
#define MAX_WAIT_PARTICIPANTS 3

typedef struct {
  heap_t        heap;
  uint8_t       *stack_memory;
  uint8_t       *arrays_memory;
  const uint8_t *code_memory;
  Context_t     context;      /* represents the parent context for now */
  Context_t     contexts[VMC_MAX_CONTEXTS];     /* Will likely change */
  bool          context_used[VMC_MAX_CONTEXTS];
  Channel_t     channels[MAX_CHANNELS]; /* Might be declared outside vmc */
} vmc_t;

extern vmc_t vm_containers[]; /* For testing, remove this later */

/**********************/
/* External Interface */
/**********************/

extern int vmc_init(void);

// These need to run within some lower level thread abstractions
extern int vmc_run(vmc_t *container);

extern int init_all_chans(Channel_t *c, uint8_t *mem); /* Could be an internal function */


#endif
