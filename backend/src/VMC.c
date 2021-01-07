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


#include <VMC.h>
#include <heap.h>
#include <CAM.h>

/* This is just an experiment and if we end up building on it, the
   range of numbers can be extended */
#if VMC_NUM_CONTAINERS >= 1 && VMC_NUM_CONTAINERS <= 2

vmc_t vm_containers[VMC_NUM_CONTAINERS];

#else
#error "VMC_NUM_CONTAINERS must be set to an integer value from 1 to 2"
#endif


#if VMC_NUM_CONTAINERS >= 1
uint8_t vmc_container_1_heap[VMC_CONTAINER_1_HEAP_SIZE_BYTES];
uint8_t vmc_container_1_stack[VMC_CONTAINER_1_STACK_SIZE_BYTES];
uint8_t vmc_container_1_arrays[VMC_CONTAINER_1_ARRAY_MEM_SIZE_BYTES];
uint8_t vmc_container_1_channels[VMC_CONTAINER_1_CHANNEL_MEM_SIZE_BYTES];

const uint8_t vmc_container_1_code[] = {
  #include VMC_CONTAINER_1_BYTECODE_FILE
  ,0
};
#endif

#if VMC_NUM_CONTAINERS >= 2
uint8_t vmc_container_2_heap[VMC_CONTAINER_2_HEAP_SIZE_BYTES];
uint8_t vmc_container_2_stack[VMC_CONTAINER_2_STACK_SIZE_BYTES];
uint8_t vmc_container_2_arrays[VMC_CONTAINER_2_ARRAY_MEM_SIZE_BYTES];

const uint8_t vmc_container_2_code[] = {
  #include VMC_CONTAINER_2_BYTECODE_FILE
  ,0
};

#endif


int vmc_init(void) {

  int r = 0;
  int rl = 0;

  #if VMC_NUM_CONTAINERS >= 1
  rl = heap_init(&vm_containers[VMC_CONTAINER_1].heap, vmc_container_1_heap, VMC_CONTAINER_1_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_1].stack_memory   = vmc_container_1_stack;
  vm_containers[VMC_CONTAINER_1].code_memory    = vmc_container_1_code;
  vm_containers[VMC_CONTAINER_1].arrays_memory  = vmc_container_1_arrays;
  init_all_chans(vm_containers[VMC_CONTAINER_1].channels, vmc_container_1_channels);
  r++;
  #endif

  #if VMC_NUM_CONTAINERS >= 2
  rl = heap_init(&vm_containers[VMC_CONTAINER_2].heap, vmc_container_2_heap, VMC_CONTAINER_2_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_2].stack_memory  = vmc_container_2_stack;
  vm_containers[VMC_CONTAINER_2].code_memory   = vmc_container_2_code;
  vm_containers[VMC_CONTAINER_2].arrays_memory = vmc_container_2_arrays;
  r++;
  #endif

  return r;
}


int vmc_run(vmc_t *container) {

  for (int i = 0; i < VMC_MAX_CONTEXTS; i++) {
    container->context_used[i] = false;
  }

  INT pc = 0;
  /* Check valid code */
  uint32_t magic = 0;
  magic |= container->code_memory[pc++] << 24; /* not sure this shifting works out */
  magic |= container->code_memory[pc++] << 16;
  magic |= container->code_memory[pc++] << 8;
  magic |= container->code_memory[pc++];

  if (magic != 0xFEEDCAFE) return 0;

  /* uint8_t version = container->code_memory[pc++]; */
  pc++;

  uint16_t pool_size_ints;
  pool_size_ints = container->code_memory[pc++] << 8;
  pool_size_ints |= container->code_memory[pc++];

  pc += (pool_size_ints * 4);

  uint16_t pool_size_strings;
  pool_size_strings = container->code_memory[pc++] << 8;
  pool_size_strings |= container->code_memory[pc++];

  pc += pool_size_strings;

  uint16_t pool_size_native;
  pool_size_native = container->code_memory[pc++] << 8;
  pool_size_native |= container->code_memory[pc++];

  pc += (pool_size_native * 4);

  uint32_t code_size;
  code_size = container->code_memory[pc++] << 24;
  code_size |= container->code_memory[pc++] << 16;
  code_size |= container->code_memory[pc++] << 8;
  code_size |= container->code_memory[pc++];

  /* Now pc should be the index of the first instruction. */
  /* set up the parent context */
  /* Running all computations in parent context for now */
  cam_value_t v_empty = get_cam_val(0,0);
  container->context.env = v_empty;
  container->context.pc  = pc;
  container->context.context_id = 0; /* Not useful here; will be used in concurrency */


  /* Start executing instructions now */
  uint8_t current_inst = container->code_memory[pc];
  while(current_inst != 13){ // stop instruction
    (*evaluators[current_inst])(container, &pc);
    if(pc == -1){
      DEBUG_PRINT(("Instruction %u failed",current_inst));
      return -1; // error
    }
    current_inst = container->code_memory[pc];  }
  /* Encountered STOP now */

  /* end */
  return 1;
}

int init_all_chans(Channel_t *c, uint8_t *mem){

  int mem_offset = 0;
  for(int i = 0; i < MAX_CHANNELS; i++){

    chan_queue_t sq = { .capacity = 0 };
    chan_queue_t rq = { .capacity = 0 };

    int sq_status = chan_q_init(&sq, &mem[mem_offset], MAX_WAIT_PARTICIPANTS);
    if(sq_status == -1){
      DEBUG_PRINT(("Failed to initialise sendq for %dth channel", i));
      return -1;
    }

    int rq_status = chan_q_init(&rq, &mem[mem_offset + MAX_WAIT_PARTICIPANTS], MAX_WAIT_PARTICIPANTS);
    if(rq_status == -1){
      DEBUG_PRINT(("Failed to initialise recvq for %dth channel", i));
      return -1;
    }

    mem_offset += 2 * MAX_WAIT_PARTICIPANTS;
    Channel_t ch = { .in_use = false };
    int ch_status = channel_init(&ch, sq, rq);
    if(ch_status == -1){
      DEBUG_PRINT(("Failed to initialise %dth channel", i));
      return -1;
    }

    c[i] = ch;
  }

  return 1;
}

/* DEBUG loop
   while(current_inst != 13){ // stop instruction
     DEBUG_PRINT(("Current instruction : %u\n\n",current_inst));
     (*evaluators[current_inst])(container, &pc);
     if(pc == -1){
       DEBUG_PRINT(("Instruction %u failed",current_inst));
       return -1; // error
     }
     current_inst = container->code_memory[pc];
     DEBUG_PRINT((" Env : %u\n\n", container->context.env.value));
     heap_show(&container->heap, 10);
     DEBUG_PRINT((" Stack pointer : %u\n\n",container->context.stack.sp));
     stack_show(&container->context.stack,5);
     DEBUG_PRINT(("\n\n"));
   }

 */
