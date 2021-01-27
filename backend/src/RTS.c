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


static UINT extract_bits(UINT value, int lsbstart, int numbits){
  unsigned mask = ( (1<<(numbits-lsbstart+1))-1) << lsbstart;
  //                                                   ^
  //                                                   |
  //                             shifts the mask such that counting `numbits`
  //                             begins from the point of the shift
  return (value & mask) >> lsbstart;
}


static int findSynchronizable(vmc_t *container, event_t *evts, base_event_t *bev){
  heap_index index = evts->event_head;
  do{

      cam_value_t base_evt_pointer = heap_fst(&container->heap, index);
      if(base_evt_pointer.flags & (1 << 15)){ // check if cell is actually a pointer
        cam_value_t base_evt_simple_cam = heap_fst(&container->heap, (heap_index)base_evt_pointer.value);
        base_event_simple_t bevt_simple =
          {   .e_type     = extract_bits(base_evt_simple_cam.value, 16, sizeof(event_type_t))
            , .context_id = extract_bits(base_evt_simple_cam.value,  8, sizeof(UUID))
            , .channel_id = extract_bits(base_evt_simple_cam.value,  0, sizeof(UUID))
          };

        if(bevt_simple.e_type == SEND){
          if(pollQ(&container->channels[bevt_simple.channel_id].recvq)){
            cam_value_t wrap_label_cam = heap_snd(&container->heap, (heap_index)base_evt_pointer.value);
            base_event_t bevt = { .bev = bevt_simple, .wrap_label = (uint16_t)wrap_label_cam.value };
            *bev = bevt;
            return 1;
          } // else continue
        } else { // recvEvt
          if(pollQ(&container->channels[bevt_simple.channel_id].sendq)){
            cam_value_t wrap_label_cam = heap_snd(&container->heap, (heap_index)base_evt_pointer.value);
            base_event_t bevt = { .bev = bevt_simple, .wrap_label = (uint16_t)wrap_label_cam.value };
            *bev = bevt;
            return 1;
          } // else continue
        }
      } else {
        DEBUG_PRINT(("Error in heap layout; Not a pointer\n"));
        return -2; //XXX: Used a different error code
      }

      cam_value_t pointer_to_next = heap_snd(&container->heap, index);
      index = (heap_index)pointer_to_next.value;

  } while(index != HEAP_NULL);

  return -1;
}

static int blockAllEvents(vmc_t *container, event_t *evts){
  heap_index index = evts->event_head;
  do{

      cam_value_t base_evt_pointer = heap_fst(&container->heap, index);
      if(base_evt_pointer.flags & (1 << 15)){ // check if cell is actually a pointer
        cam_value_t base_evt_simple_cam = heap_fst(&container->heap, (heap_index)base_evt_pointer.value);
        base_event_simple_t bevt_simple =
          {   .e_type     = extract_bits(base_evt_simple_cam.value, 16, sizeof(event_type_t))
            , .context_id = extract_bits(base_evt_simple_cam.value,  8, sizeof(UUID))
            , .channel_id = extract_bits(base_evt_simple_cam.value,  0, sizeof(UUID))
          };

        if(bevt_simple.e_type == SEND){
          //XXX: The current context's id is required here
          /* The Context_t context; field should instead be
           * UUID current_runnting_context; and then we can take that context id and enqueue that
           */
          // int j = q_enqueue(&container->channels[bevt_simple.channel_id].sendq, container->current_running_context_id);

        } else { // recvEvt
          // int j = q_enqueue(&container->channels[bevt_simple.channel_id].recvq, container->current_running_context_id);
        }
      } else {
        DEBUG_PRINT(("Error in heap layout; Not a pointer\n"));
        return -2; //XXX: Used a different error code
      }

      cam_value_t pointer_to_next = heap_snd(&container->heap, index);
      index = (heap_index)pointer_to_next.value;

  } while(index != HEAP_NULL);

  return 1;
}


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
      container->contexts[i].env =
        container->contexts[container->current_running_context_id].env; //copying the environment
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

static int dispatch(vmc_t *container){
  UUID context_id = 0;
  int de_q_status = q_dequeue(&container->rdyQ, &context_id);
  if (de_q_status == -1){
    DEBUG_PRINT(("Ready Queue is empty\n"));
    return -1; // This is the standard state of a microcontroller
               // and it should sleep when it gets -1 on dispatch
  }
  container->current_running_context_id = context_id;
  return 1;


  /* Before current_running_context_id was introduced */
  /* container->context = container->contexts[context_id]; // This will overwrite the parent context; */
  /*                                                       // Do we want to store it somewhere? */
  /* container->context.env = container->contexts[context_id].env; */
  /* container->context.pc = container->contexts[context_id].pc; */
  //hopefully stack is set by the first container->context = ....

}

int sync(vmc_t *container, event_t *evts){
  base_event_t bev = { .wrap_label = 0 };
  int i = findSynchronizable(container, evts, &bev);
  /*
   * if i is -1 call block on all evts and do dispatch
   * if i is  1 then call doFn
   */
  if(i == 1){
    //doFn
  } else if (i == -1) {
    int j = blockAllEvents(container, evts);
    if(j == -1){
      DEBUG_PRINT(("Block events failed! \n"));
      return -1;
    }
    dispatch(container);
  } else {
    // error
  }
  return 1;
}

