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

#ifndef __TRUSTEDVMC_H_
#define __TRUSTEDVMC_H_

#include <vm-conf.h>
#include <typedefs.h>
#include <register.h>
#include <heap.h>
#include <queue.h>
#include <channel.h>
#include <priorityqueue.h>
#include <Context.h>
#include <ll/ll_driver.h>
#include <stdint.h>

#define VMC_CONTAINER_1 0
#define VMC_CONTAINER_2 1

#define VMC_MAX_CONTEXTS 4
#define VMC_MAX_DRIVERS  16
#define CONTEXT_STACK_SPACE 256

#define MAX_CHANNELS 100 // This number should be configurable or statically analyzable from the code
#define MAX_WAIT_PARTICIPANTS 3

typedef struct {
  heap_t        heap;
  uint8_t       *stack_memory;
  uint8_t       *arrays_memory;
  const uint8_t *code_memory;
  UINT          code_size;
  UUID          current_running_context_id;
  Context_t     contexts[VMC_MAX_CONTEXTS];     /* Will likely change */
  bool          context_used[VMC_MAX_CONTEXTS];
  Channel_t     channels[MAX_CHANNELS]; /* Might be declared outside vmc */
  PriorityQ_t   rdyQ;
  PriorityQ_t   waitQ;
  ll_driver_t   drivers[VMC_MAX_DRIVERS];
  void*         backend_custom; /* Can be used by a backend for low level integration */
  bool          all_contexts_stopped;

} vmc_trusted_t;


/****************************************************/
/* low-level message queue interface function types */ 

#define VMC_MESSAGE_RECEIVED 0
#define VMC_NO_MESSAGE       -1 

/**********************/
/* External Interface */
/**********************/
extern int vmc_init_trusted(vmc_trusted_t *vm_containers, int max_num_containers);

extern int vmc_run_trusted(vmc_trusted_t *container,void (*dbg_print)(const char *str, ...));

extern heap_index vmc_heap_alloc_withGC_trusted(vmc_trusted_t *container);

/* 
   Allocate n elements from the heap, returned as a cons list. 
   Succeeds to allocate n elements or fails. No partial success.
 */ 
extern heap_index vmc_heap_alloc_n_trusted(vmc_trusted_t *container, unsigned int n);

#endif
