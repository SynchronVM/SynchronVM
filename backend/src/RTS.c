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


static int findSynchronizable(vmc_t *container, event_t *evts, cam_event_t *cev){
  heap_index index = evts->event_head;
  do{

      cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);

      cam_value_t base_evt_cam =
        heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

      cam_value_t message =
        heap_snd(&container->heap, (heap_index)cam_evt_pointer.value);


      base_event_t base_evt =
        {   .e_type = extract_bits(base_evt_cam.value, 24, sizeof(event_type_t))
          , .channel_id = extract_bits(base_evt_cam.value, 16, sizeof(UUID))
          , .wrap_label = extract_bits(base_evt_cam.value,  0, sizeof(uint16_t))
        };

      cam_event_t cevt = { .bevt = base_evt, .msg = message };

      if(base_evt.e_type == SEND){

        if(pollQ(&container->channels[base_evt.channel_id].recvq)){
          *cev = cevt;
          return 1;
        } // else continue the do-while loop

      } else { // recvEvt

        if(pollQ(&container->channels[base_evt.channel_id].sendq)){
          *cev = cevt;
          return 1;
        } // else continue the do-while loop

      }


      cam_value_t pointer_to_next = heap_snd(&container->heap, index);
      index = (heap_index)pointer_to_next.value;

  } while(index != HEAP_NULL);

  return -1;
}

static int blockAllEvents(vmc_t *container, event_t *evts){
  heap_index index = evts->event_head;
  do{

      cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);

      cam_value_t base_evt_cam =
        heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

      // the message field is not used while blocking

      base_event_t bevt =
        {   .e_type = extract_bits(base_evt_cam.value, 24, sizeof(event_type_t))
          , .channel_id = extract_bits(base_evt_cam.value, 16, sizeof(UUID))
          , .wrap_label = extract_bits(base_evt_cam.value,  0, sizeof(uint16_t))
        };

      if(bevt.e_type == SEND){
        int j =
          q_enqueue(  &container->channels[bevt.channel_id].sendq
                    , container->current_running_context_id);
        if(j == -1){
          DEBUG_PRINT(( "Cannot enqueue in channel %u 's send queue \n"
                       , bevt.channel_id));
          return -1;
        }

      } else { // recvEvt
        int j = q_enqueue(&container->channels[bevt.channel_id].recvq, container->current_running_context_id);
        if(j == -1){
          DEBUG_PRINT((" Cannot enqueue in channel %u 's recv queue \n"
                       , bevt.channel_id));
          return -1;
        }

      }


      cam_value_t pointer_to_next = heap_snd(&container->heap, index);
      index = (heap_index)pointer_to_next.value;

  } while(index != HEAP_NULL);

  return 1;
}


int channel(vmc_t *container, UUID *chan_id){
  for(int i = 0; i < MAX_CHANNELS; i++){
    if(container->channels[i].in_use == false){
      container->channels[i].in_use = true;
      *chan_id = (UUID)i;
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
      int j = q_enqueue(&container->rdyQ, i);
      if(j == -1){
        DEBUG_PRINT(("Cannot enqueue in ready queue \n"));
        return -1;
      }
      // eval_RTS_spawn should now simply do *pc_idx++
      // so that the parent context can continue running
      return 1;
    }
  }
  DEBUG_PRINT(("Cannot spawn more threads \n"));
  return -1;
}

static int dispatch(vmc_t *container){
  UUID context_id;
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

static int synchronizeNow(vmc_t *container, cam_event_t cev){
  /* NOTE: BEWARE! SEND and RECV have different behaviours! Study */
  /* both the if and else blocks carefully to understand the */
  /* difference. In both cases the receiving thread starts executing */
  /* when `sync` succeeds! Therefore the code is differnt if you */
  /* view it from the perspective of the sender or the receiver. */

  base_event_t bev = cev.bevt;

  if(bev.e_type == SEND){

    UUID recv_context_id;
    int deq_status =
      q_dequeue(&container->channels[bev.channel_id].recvq, &recv_context_id);
    if(deq_status == -1){ //empty queue
      DEBUG_PRINT(( "Recv Queue of %u empty for syncing send \n"
                   , bev.channel_id));
      return -1;
    }

    int rem_status =
      q_remove(&container->channels[bev.channel_id].recvq, &recv_context_id);
    if(rem_status == -1){
      DEBUG_PRINT((  "Failed to remove %u from channel %u 's recv queue \n"
                   , recv_context_id
                   , bev.channel_id));
      return -1;
    }
    /* NOTE Message passing begins */

    /*
     * Put the message on the receiving context's env register
     */

    container->contexts[recv_context_id].env = cev.msg;

    //XXX: PC_IDX should be moved to bev.wrap_label here and the
    // wrapped function should be applied now

    /* NOTE Message passing ends */

    int enq_status =
      q_enqueue(&container->rdyQ, container->current_running_context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }

    // the receiving thread will run now
    container->current_running_context_id = recv_context_id;

    return 1;

  } else {

    UUID send_context_id;
    int deq_status =
      q_dequeue(&container->channels[bev.channel_id].sendq, &send_context_id);
    if(deq_status == -1){ //empty queue
      DEBUG_PRINT((  "Send Queue of %u empty for syncing recv \n"
                   , bev.channel_id));
      return -1;
    }

    int rem_status =
      q_remove(&container->channels[bev.channel_id].sendq, &send_context_id);
    if(rem_status == -1){
      DEBUG_PRINT((  "Failed to remove %u from channel %u 's send queue \n"
                   , send_context_id
                   , bev.channel_id));
      return -1;
    }
    /* NOTE Message passing begins */
    /*
     * Place the message on the receivers env (current running context)
     */

    container->contexts[container->current_running_context_id].env = cev.msg;

    //XXX: PC_IDX should be moved to bev.wrap_label here and the
    // wrapped function should be applied now

    /* NOTE Message passing ends */

    int enq_status = q_enqueue(&container->rdyQ, send_context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }

    // the receiver will automatically run because we are now executing its context
    return 1;

  }
}

int sync(vmc_t *container, event_t *evts){
  cam_event_t cev;
  int i = findSynchronizable(container, evts, &cev);

  if(i == 1){

    int sync_status = synchronizeNow(container, cev);
    if(sync_status == -1){
      DEBUG_PRINT(("Synchronization failed! \n"));
      return -1;
    }

  } else {

    int j = blockAllEvents(container, evts);
    if(j == -1){
      DEBUG_PRINT(("Block events failed! \n"));
      return -1;
    }
    dispatch(container);

  }

  return 1;
}

int sendEvt(vmc_t *container, UUID *chan_id, cam_value_t msg, event_t *sevt){
  base_event_t bev = { .e_type = SEND, .channel_id = *chan_id };
  // create a UINT by setting bev members correctly and then heap_alloc
  return -1;
}

int recvEvt(vmc_t *container, UUID *chan_id, event_t *revt){
  return -1;
}
