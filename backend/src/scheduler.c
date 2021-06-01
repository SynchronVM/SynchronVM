/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Abhiroop Sarkar, Joel Svensson             		  */
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

#include <scheduler.h>
#include <heap.h>
#include <CAM.h>
#include <RTS.h>
#include <queue.h>

//#define TRACE_ON 
#define MAX_TRACE_LENGTH 100

typedef struct scheduler_trace_s { 
  UUID context_id;
  cam_register_t env;
  UINT pc;
  uint8_t instr;
  unsigned int sp;
  uint32_t num_msgs;
  struct scheduler_trace_s *next; 
} scheduler_trace_t; 

scheduler_trace_t trace_storage[MAX_TRACE_LENGTH];
int trace_next = 0;

scheduler_trace_t *trace = NULL;

void trace_add(UUID cid, cam_register_t env, UINT pc,
	       uint8_t instr, unsigned int sp, uint32_t num_msgs) {

  if (trace_next == 100) { 
    trace_next = 0;
  }

  scheduler_trace_t *curr = &trace_storage[trace_next];

  curr->context_id = cid;
  curr->env = env;
  curr->pc = pc;
  curr->instr = instr;
  curr->sp = sp;
  curr->next = trace;
  curr->num_msgs = num_msgs;
  trace = curr;
  trace_next++;
  trace_storage[trace_next].next = NULL;
}
	       


void trace_print(void (*dbg_print)(const char *str, ...), int num) {

  scheduler_trace_t *curr = trace;

  int n = 0; 
  
  while (curr) {
    dbg_print("*****************************************************\r\n");    
    dbg_print("Trace position: %d \r\n", n++);
    dbg_print("Context: %d\r\n",curr->context_id );
    dbg_print("PC: %d\r\n", curr->pc);
    dbg_print("Environment: %u\r\n", curr->env.value);
    dbg_print("Instruction: 0x%x\r\n", curr->instr);
    dbg_print("Msg num: %u\r\n", curr->num_msgs);
    curr = curr->next;
    if (n > num) break;
  }
}



int scheduler(vmc_t *container,
	      message_read_poll_fun poll_msg,
	      message_read_block_fun block_msg,
	      message_queue_num_used_fun msgq_num_used,
        void (*dbg_print)(const char *str, ...)) {

  //type: poll_msg(vmc_t *vmc, ll_driver_msg_t *msg);
  //type: block_msg(vmc_t *vmc, ll_driver_msg_t *msg);

  (void)poll_msg;

  ll_driver_msg_t msg;

  dbg_print("Entered Scheduler\r\n");
  dbg_print("container address: %u\r\n", (uint32_t)container);

  while (true) {


#ifdef TRACE_ON 
    uint32_t num_msgs = msgq_num_used(container);
    
    trace_add(container->current_running_context_id,
	      container->contexts[container->current_running_context_id].env,
	      container->contexts[container->current_running_context_id].pc,
	      container->code_memory[container->contexts[container->current_running_context_id].pc],
	      stack_get_sp(&container->contexts[container->current_running_context_id].stack),
	      num_msgs);
#endif	      
	     
    /* if (container->current_running_context_id != UUID_NONE) { */

    /*   dbg_print("[%d] stack sp: %u\r\n", */
    /* 		container->current_running_context_id, */
    /* 		stack_get_sp(&container->contexts[container->current_running_context_id].stack)); */
    /* } */
    
    //dbg_print("CURRENT_ID: %u\r\n", container->current_running_context_id);

    if(container->all_contexts_stopped){
      // All of the contexts have encountered the STOP operation; Program STOP
      break;
    }

    /* What if we want to pre-emt running context for an important 
       message 
       - threads cooperative. 
       - 
    */
    /* If we are doing nothing, block on the message queue */
    if (container->current_running_context_id == UUID_NONE) {

      block_msg(container, &msg);
      /* dbg_print("message received: blocking\r\n"); */
      /* dbg_print("  driver: %u\r\n", msg.driver_id); */
      /* dbg_print("  msg_typ: %u\r\n", msg.msg_type); */
      /* dbg_print("  data: %u\r\n", msg.data); */
      /* dbg_print("  time: %llu\r\n", msg.timestamp); */
      /* handle msg */
      handle_msg(container, &msg);
      /* while (poll_msg(container, &msg) == 0) { */
      /* 	dbg_print("message received: poll loop in blocking\r\n"); */
      /* 	dbg_print("  driver: %u\r\n", msg.driver_id); */
      /* 	dbg_print("  msg_typ: %u\r\n", msg.msg_type); */
      /* 	dbg_print("  data: %u\r\n", msg.data); */
      /* 	dbg_print("  time: %llu\r\n", msg.timestamp); */
      /* 	/\* handle msg *\/ */
      /* 	handle_msg(container, &msg); */
      /* 	/\* This should be the same handler as below, do not context-switch*\/ */
      /* 	//handle_msg(container, &msg); */
      /*   /\*handle messages*\/ */
      /*   /\* enqueue processes *\/ */
      /* } */
    }

    /* If a context is running do this... */
    if (container->current_running_context_id != UUID_NONE) {

      INT *pc = (INT *)&container->contexts[container->current_running_context_id].pc;
      //dbg_print("*****************************************************\r\n");
      /* dbg_print("executing ctx: %d\r\n", container->current_running_context_id); */
      /* dbg_print("ctx pc: %d\r\n", container->contexts[container->current_running_context_id].pc); */
      /* dbg_print("pc    : %d\r\n", *pc); */
      /* dbg_print("current env: %u\r\n", container->contexts[container->current_running_context_id].env.value); */
      /* dbg_print("current instr: 0x%x\r\n", container->code_memory[*pc]); */
      /* dbg_print("sizeof(evaluators) = %d\r\n", sizeof(evaluators)); */

      /* Execute an instruction */

      uint8_t current_inst = container->code_memory[*pc];

      if (current_inst > (sizeof(evaluators) / 4)) {
        dbg_print("current_inst is invalid\r\n"); 
	/* dbg_print("*****************************************************\r\n"); */
	/* dbg_print("executing ctx: %d\r\n", container->current_running_context_id); */
	/* dbg_print("ctx pc: %d\r\n", container->contexts[container->current_running_context_id].pc); */
	/* dbg_print("pc    : %d\r\n", *pc); */
	/* dbg_print("current env: %u\r\n", container->contexts[container->current_running_context_id].env.value); */
	/* dbg_print("current instr: 0x%x\r\n", container->code_memory[*pc]); */
	/* dbg_print("sizeof(evaluators) = %d\r\n", sizeof(evaluators)); */
#ifdef TRACE_ON
	trace_print(dbg_print, 25);
#endif
	return -1;
      } else {
        evaluators[current_inst](container, pc);
      }



      if(*pc  == -1){
        dbg_print("Instruction %u failed",current_inst);
#ifdef TRACE_ON
	trace_print(dbg_print, 25);
#endif	
        return -1; // error
      }

    }
  }
  /* end */
  dbg_print("Closing down scheduler\r\n");
  return 1;
}
