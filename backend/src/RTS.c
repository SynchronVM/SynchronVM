/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar                            		  */
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

#include <RTS.h>


int channel(vmc_t *container, Channel_t *chan){
  for(int i = 0; i < MAX_CHANNELS; i++){
    if(container->channels[i].in_use == false){
      container->channels[i].in_use = true;
      *chan = container->channels[i];
      return 1;
    }
  }
  DEBUG_PRINT(("All channels in current container in use \n"));
  return -1;
}

int spawn(vmc_t *container, uint16_t label){
  for(int i = 0; i < VMC_MAX_CONTEXTS; i++){
    if(container->context_used[i] == false){
      container->contexts[i].pc = (UINT)label;
      container->contexts[i].env = container->context.env; //copying the environment
      // XXX
      //container->contexts[i].stack = Case 1. statically allocated in vmc_init; nothing to be done here
      //                               Case 2. some kind of malloc with GC for stack memory with optimal memory use
      //                                       In Case 2 we have to call the stack_alloc function here
      container->context_used[i] = true;
      q_enqueue(&container->rdyQ, i);
      // eval_RTS_spawn should now simply do *pc_idx++
      // so that the parent context can continue running
      return 1;
    }
  }
  DEBUG_PRINT(("Cannot spawn more threads \n"));
  return -1;
}

int dispatch(vmc_t *container){
  UUID context_id = 0;
  int de_q_status = q_dequeue(&container->rdyQ, &context_id);
  if (de_q_status == -1){
    DEBUG_PRINT(("Ready Queue is empty\n"));
    return -1; // This is the standard state of a microcontroller
               // and it should sleep when it gets -1 on dispatch
  }
  container->context = container->contexts[context_id]; // This will overwrite the parent context;
                                                        // Do we want to store it somewhere?
  return 1;

}
