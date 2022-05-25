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

#include <SVM_DEBUG.h>

#include <trusted/Trustedscheduler.h>
#include <heap.h>
#include <trusted/TrustedCAM.h>
#include <trusted/TrustedRTS.h>
#include <queue.h>

static void initLogicalTime(vmc_trusted_t *vmc){
  Time currentTicks = sys_time_get_current_ticks();
  for(int i = 0; i < VMC_MAX_CONTEXTS; i ++){
    vmc->contexts[i].logicalTime = currentTicks;
  }
}



/*************/
/* Scheduler */
int scheduler_trusted(vmc_trusted_t *container) {

  //type: poll_msg(vmc_t *vmc, svm_msg_t *msg);
  //type: block_msg(vmc_t *vmc, svm_msg_t *msg);

  svm_msg_t msg;

  DEBUG_PRINT(("Entered Scheduler\r\n"));
  DEBUG_PRINT(("container address: %u\r\n", (uint32_t)container));


  // set logical time
  initLogicalTime(container);


  while (true) {

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
      /*handle msg */
      int msg_r = handle_msg_trusted(container, &msg);
      if (msg_r  <= 0) {
        DEBUG_PRINT(("Error in handle_msg: %d\r\n",msg_r));

        return -1;
	/* continue as if nothing has happend.
	   This should be like throwing the message away */
      }

      /* else { */
      /* 	uint32_t baseline_low; */
      /* 	uint32_t baseline_high; */
      /* 	uint32_t t_low; */
      /* 	uint32_t t_high; */
      /* 	Time t = sys_time_get_current_ticks(); */
      /* 	t_low = t; */
      /* 	t_high = t >> 32; */
      /* 	baseline_low = container->contexts[container->current_running_context_id].logicalTime; */
      /* 	baseline_high = container->contexts[container->current_running_context_id].logicalTime >> 32; */

      /* 	dbg_print("Current ctx: %d   (Baseline: %u, %u) (t: %u, %u)\r\n", */
      /* 		  container->current_running_context_id, */
      /* 		  baseline_high,baseline_low, t_high, t_low); */
      /* } */

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
      //dbg_print("%x\r\n", (uint32_t)pc);

      /* dbg_print("*****************************************************\r\n"); */
      /* dbg_print("executing ctx: %d\r\n", container->current_running_context_id); */
      /* dbg_print("ctx pc: %d\r\n", container->contexts[container->current_running_context_id].pc); */
      /* dbg_print("pc    : %d\r\n", *pc); */
      /* dbg_print("sp    : %d\r\n", stack_get_sp(&container->contexts[container->current_running_context_id].stack)); */
      /* dbg_print("current env: %u\r\n", container->contexts[container->current_running_context_id].env.value); */
      /* dbg_print("current instr: 0x%x  [Dec: %u]\r\n", container->code_memory[*pc], container->code_memory[*pc]); */
      /* dbg_print("sizeof(evaluators) = %d\r\n", sizeof(evaluators)); */

      /* Execute an instruction */

      uint8_t current_inst = container->code_memory[*pc];
      /* if (current_inst == 55){ */
      /*   uint8_t next_inst = container->code_memory[*pc+1]; */
      /*   DEBUG_PRINT(("%u %u\n", current_inst, next_inst)); */
      /* } else { */
      /*   DEBUG_PRINT(("%u\n", current_inst)); */
      /* } */

      if (current_inst > (sizeof(evaluators) / 4)) {

        DEBUG_PRINT(("current_inst = %u at pc = %d is invalid   (ctx = %u) \r\n", current_inst, *pc, container->current_running_context_id));
        return -1;
      } else {
        evaluators[current_inst](container, pc);
      }

      if(*pc  == -1){
        DEBUG_PRINT(("Instruction %u failed",current_inst));
        return -1; // error
      }
    }
  }
  /* end */
  DEBUG_PRINT(("Closing down scheduler\r\n"));
  return 1;
}
