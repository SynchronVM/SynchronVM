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
#include <queue.h>

#include <ll/ll_driver.h>

/* This is just an experiment and if we end up building on it, the
   range of numbers can be extended */
#if VMC_NUM_CONTAINERS >= 1 && VMC_NUM_CONTAINERS <= 2

/* vmc_t vm_containers[VMC_NUM_CONTAINERS]; */
/* Removing the containers from here and letting the lower level system
   take care of organising storage space for these */

#else
#error "VMC_NUM_CONTAINERS must be set to an integer value from 1 to 2"
#endif


#if VMC_NUM_CONTAINERS >= 1
uint8_t vmc_container_1_heap[VMC_CONTAINER_1_HEAP_SIZE_BYTES];
uint8_t vmc_container_1_stack[VMC_CONTAINER_1_STACK_SIZE_BYTES];
uint8_t vmc_container_1_arrays[VMC_CONTAINER_1_ARRAY_MEM_SIZE_BYTES];
uint8_t vmc_container_1_channels[VMC_CONTAINER_1_CHANNEL_MEM_SIZE_BYTES];
uint8_t vmc_container_1_rdyq[sizeof(UUID) * VMC_MAX_CONTEXTS];

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


/* we should rewrite this code so that we can
   macro_instantiate the whole thing for up
   to N containers. */

int vmc_init(vmc_t *vm_containers, int max_num_containers) {

  int r = 0;
  int rl = 0;
  uint32_t drv_num = 0;

  if (VMC_NUM_CONTAINERS > max_num_containers) {
    return -1; /* error! */
  }

  #if VMC_NUM_CONTAINERS >= 1
  rl = heap_init(&vm_containers[VMC_CONTAINER_1].heap, vmc_container_1_heap, VMC_CONTAINER_1_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_1].stack_memory   = vmc_container_1_stack;
  vm_containers[VMC_CONTAINER_1].code_memory    = vmc_container_1_code;
  vm_containers[VMC_CONTAINER_1].arrays_memory  = vmc_container_1_arrays;
  vm_containers[VMC_CONTAINER_1].current_running_context_id = 0;
  init_all_chans(  vm_containers[VMC_CONTAINER_1].channels
                 , vmc_container_1_channels);
  init_all_contextstacks(  vm_containers[VMC_CONTAINER_1].contexts,
			   vm_containers[VMC_CONTAINER_1].stack_memory,
			   VMC_CONTAINER_1_STACK_SIZE_BYTES);
  Queue_t readyq = { .capacity = 0 };
  int readyq_status = q_init(&readyq, vmc_container_1_rdyq, VMC_MAX_CONTEXTS);
  if(readyq_status == -1){
    DEBUG_PRINT(("Failed to initialise ready queue"));
    return -1;
  }
  vm_containers[VMC_CONTAINER_1].rdyQ  = readyq;


  /**********************************************************/
  /* Initialize the Drivers
     At this point we can give the vmc_t data to the driver.
     Maybe we just need to give the "backend_custom" field
     to the driver. The LL-layer will just pass this information
     forward to the os-specific layer that can cast it to the
     correct os specific datastructure.
  */

  ll_driver_t lld;

  /* it is fine to include (#include ll_uart.h) the ll drivers any number of times */
  #if VMC_CONTAINER_1_USE_UART_0
  /* #include <ll/ll_uart.h>     */

  drv_num++;
  #endif

  #if VMC_CONTAINER_1_USE_BUTTON_0
  #include <ll_button.h>
  if (ll_button_init(&lld, drv_num, vm_containers[VMC_CONTAINER_1].backend_custom, 0)) {
    vm_containers[VMC_CONTAINER_1].drivers[drv_num] = lld;
    drv_num++;
  }
  #endif

  #if VMC_CONTAINER_1_USE_LED_0
  #include <ll_led.h>

  drv_num++:
  #endif

  r++;
  #endif

  #if VMC_NUM_CONTAINERS >= 2
  rl = heap_init(&vm_containers[VMC_CONTAINER_2].heap, vmc_container_2_heap, VMC_CONTAINER_2_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_2].stack_memory  = vmc_container_2_stack;
  vm_containers[VMC_CONTAINER_2].code_memory   = vmc_container_2_code;
  vm_containers[VMC_CONTAINER_2].arrays_memory = vmc_container_2_arrays;
  vm_containers[VMC_CONTAINER_2].current_running_context_id = 0;
  // channel initialization missing
  r++;
  #endif

  return r;
}


int vmc_run(vmc_t *container,void (*dbg_print)(const char *str, ...)) {


  dbg_print("vcm_run container address: %u\r\n", (uint32_t)container);

  for (int i = 0; i < VMC_MAX_CONTEXTS; i++) {
    container->context_used[i] = false; //XXX: should move to vmc_init
  }

  INT pc = 0;
  /* Check valid code */
  uint32_t magic = 0;
  magic |= ((uint32_t)container->code_memory[pc++]) << 24; /* not sure this shifting works out */
  magic |= ((uint32_t)container->code_memory[pc++]) << 16;
  magic |= ((uint32_t)container->code_memory[pc++]) << 8;
  magic |= ((uint32_t)container->code_memory[pc++]);


  /* feedcafe: 4276996862 */
  /* magic:    1214606444 */
  dbg_print("magic: %u\r\n", magic);
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

  /* cam_value_t v_empty = get_cam_val(0,0); */
  /* //container->current_running_context_id = 0; // done by the scheduler */
  /* container->contexts[container->current_running_context_id].env = v_empty; */
  /* container->contexts[container->current_running_context_id].pc  = pc; */


  /* /\* Start executing instructions now *\/ */
  /* uint8_t current_inst = container->code_memory[pc]; */
  /* while(current_inst != 13){ // stop instruction */
  /*   (*evaluators[current_inst])(container, &pc); */
  /*   if(pc == -1){ */
  /*     DEBUG_PRINT(("Instruction %u failed",current_inst)); */
  /*     return -1; // error */
  /*   } */
  /*   current_inst = container->code_memory[pc];  } */
  /* /\* Encountered STOP now *\/ */

  /* /\* end *\/ */
  /* return 1; */


  /* Experiments with the scheduler */
  cam_value_t v_empty = get_cam_val(0,0);


  /* Set up the parent context to be active */
  container->contexts[0].env = v_empty;
  container->contexts[0].pc  = pc;
  container->context_used[0] = true;
  container->current_running_context_id = 0;


  dbg_print("vmc_run executing ctx: %d\r\n", container->current_running_context_id);
  dbg_print("vmc_run ctx pc: %d\r\n", container->contexts[container->current_running_context_id].pc);
  dbg_print("vmc_run current env: %u\r\n", container->contexts[container->current_running_context_id].env.value);
  dbg_print("vmc_run current instr: 0x%x\r\n", container->code_memory[pc]);

  /* Currently no process is running */
  //container->current_running_context_id = UUID_NONE;

  /* Enqueue the parent context as ready to run */
  /* q_enqueue(&container->rdyQ, 0); */ //XXX: rdyQ not initialised in the tests

  return 1; /* Maybe have some error codes in relation to this fun */
}

int scheduler(vmc_t *container,
	      message_read_poll_fun poll_msg,
	      message_read_block_fun block_msg,
	      void (*dbg_print)(const char *str, ...)) {


  //type: poll_msg(vmc_t *vmc, ll_driver_msg_t *msg);
  //type: block_msg(vmc_t *vmc, ll_driver_msg_t *msg);

  ll_driver_msg_t msg;

  dbg_print("Entered Scheduler\r\n");
  dbg_print("container address: %u\r\n", (uint32_t)container);

  while (true) {


    /* If we are doing nothing, block on the message queue */
    if (container->current_running_context_id == UUID_NONE) {
      block_msg(container, &msg);
      dbg_print("message received: blocking\r\n");
      /* handle msg */
      while (poll_msg(container, &msg) == 0) {
	/*handle messages*/
	/* enqueue processes */
      }
    }

    /* Every now and then poll for messages (maybe not every iteration?)*/
    if (poll_msg(container, &msg) == 0) {  /* loop over queue here ? */
      dbg_print("message received: polling\r\n");
      /* handle message */
      /* enqueue processes */
    }

    /* If a context is running do this... */
    if (container->current_running_context_id != UUID_NONE) {

      INT *pc = &container->contexts[container->current_running_context_id].pc;
      dbg_print("*****************************************************'\r\n");
      dbg_print("executing ctx: %d\r\n", container->current_running_context_id);
      dbg_print("ctx pc: %d\r\n", container->contexts[container->current_running_context_id].pc);
      dbg_print("pc    : %d\r\n", *pc);
      dbg_print("current env: %u\r\n", container->contexts[container->current_running_context_id].env.value);
      dbg_print("current instr: 0x%x\r\n", container->code_memory[*pc]);
      dbg_print("sizeof(evaluators) = %d\r\n", sizeof(evaluators));
      /* Execute an instruction */
      uint8_t current_inst = container->code_memory[*pc];

      if (current_inst > (sizeof(evaluators) / 4)) {
	dbg_print("current_inst is invalid\r\n");
      } else {
	evaluators[current_inst](container, pc);
      }

      /* Maybe there needs to be some communication between the evaluators
	 and the scheduler. Maybe by return value? or by values in the context
	 See if need for being able to "flag" about certain things arises.
      */


      if(*pc  == -1){
	DEBUG_PRINT(("Instruction %u failed",current_inst));
	return -1; // error
      }

      current_inst = container->code_memory[*pc];

      dbg_print("PC after: %d\r\n", *pc);
      dbg_print("inst after: %d\r\n", current_inst);
      dbg_print("env after: %u\r\n", container->contexts[container->current_running_context_id].env.value);

      if (current_inst == 13) {
	container->current_running_context_id = UUID_NONE;
	dbg_print("end of instruction stream\r\n");
      /* instruction 13 must be handled somehow.
	 Move context away from any ready q etc.
      */
      }
    }
  }
  /* end */
  return 1;
}


int init_all_chans(Channel_t *c, uint8_t *mem){

  int mem_offset = 0;
  for(int i = 0; i < MAX_CHANNELS; i++){

    chan_send_queue_t sq;
    chan_recv_queue_t rq;

    int sq_status = chan_send_q_init(&sq, &mem[mem_offset], MAX_WAIT_PARTICIPANTS);
    if(sq_status == -1){
      DEBUG_PRINT(("Failed to initialise sendq for %dth channel", i));
      return -1;
    }

    int rq_status =
      chan_recv_q_init(&rq, &mem[mem_offset + MAX_WAIT_PARTICIPANTS], MAX_WAIT_PARTICIPANTS);
    if(rq_status == -1){
      DEBUG_PRINT(("Failed to initialise recvq for %dth channel", i));
      return -1;
    }

    mem_offset +=   (MAX_WAIT_PARTICIPANTS * 6)  // sendq is 6 bytes each
                  + (MAX_WAIT_PARTICIPANTS * 2); // recvq is 2 bytes each
    Channel_t ch;
    int ch_status = channel_init(&ch, sq, rq);
    if(ch_status == -1){
      DEBUG_PRINT(("Failed to initialise %dth channel", i));
      return -1;
    }

    c[i] = ch;
  }

  return 1;
}

int init_all_contextstacks(Context_t *ctx, uint8_t *mem, uint32_t memory_size){

  /* Maybe we want a different number of max contexts on different VMC. 
     I think VMC_CONTAINERX_MAX_CONTEXTS should be defined in vm-conf. 
     Then also VMC_CONTAINERX_CONTEXT_STACK_SPACE could be defined in vm-conf. */
  
  if (VMC_MAX_CONTEXTS * CONTEXT_STACK_SPACE > memory_size) {
    return -1; /* Not enough space for that many stacks */
  }
      
  for(int i = 0; i < VMC_MAX_CONTEXTS; i++){

    int st_status = stack_init(  &ctx[i].stack
				 , (mem + i * CONTEXT_STACK_SPACE)
				 ,   CONTEXT_STACK_SPACE);

    if(!st_status){
      DEBUG_PRINT(("Failed to initialise stack for %dth context", i));
      return -1;
    }

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


static inline void mark_heap_context(Context_t *context, heap_t *heap){
  /* GC Roots - env register, the full stack */

  // run mark from env
  heap_mark(heap, context->env);

  // run mark for each element of the stack
  for(unsigned int i = 0; i < context->stack.size; i++){
    cam_value_t cv =
      get_cam_val(context->stack.data[i], context->stack.flags[i]);
    heap_mark(heap, cv);
  }
}


heap_index heap_alloc_withGC(vmc_t *container) {

  heap_index hi = heap_allocate(&container->heap);
  if(hi == HEAP_NULL){
    // heap full; time to do a GC

    /* GC parent context */
    mark_heap_context
      (  &container->contexts[container->current_running_context_id]
         , &container->heap);

    /* GC all active child contexts; children starts from 1 */
    for(int i = 1; i < VMC_MAX_CONTEXTS; i++){
      if(container->context_used[i] ){
        mark_heap_context(&container->contexts[i], &container->heap);
      }
    }

    //XXX: Tests breaking because channels not initialised in the tests
    //     PLEASE UNCOMMENT BELOW
    //GC all the dirty flags associated with the channels
    /* for(int i = 0; i < MAX_CHANNELS; i++){ */
    /*   if(container->channels[i].in_use){ */
    /*     // first check if channel is in use */
    /*     // and then mark all live dirty flags */
    /*     // in the sendq and then in the recvq */

    /*     for(int j = 0; j < container->channels[i].sendq.size; j++){ */
    /*       heap_mark(  &container->heap */
    /*                 , container->channels[i].sendq.data[j].dirty_flag_pointer); */
    /*     } */

    /*     for(int j = 0; j < container->channels[i].recvq.size; j++){ */
    /*       heap_mark(  &container->heap */
    /*                   , container->channels[i].recvq.data[j].dirty_flag_pointer); */
    /*     } */
    /*   } */
    /* } */

    // First phase mark complete; try allocating again
    // Sweeping is lazy and integrated into the allocator


    // if heap_allocate_helper returns HEAP_NULL again need to resize heap
    return heap_allocate(&container->heap);
  }

  return hi;
}
