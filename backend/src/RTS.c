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
#include <stdbool.h>


static inline UINT extract_bits(UINT value, int lsbstart, int numbits){
  // counting begins with 0
  //  Bit pattern -> 0 1 0 0 1 1
  //  Index       -> 5 4 3 2 1 0
  // counting always moves towards left
  UINT mask = (1 << numbits) - 1;
  return ( mask & (value >> lsbstart));
}


static inline UINT set_first_16_bits(uint8_t first8bits, uint8_t second8bits){

  UINT value = 0;
  value = value | (first8bits << 24);
  value = value | (second8bits << 16);

  return value;

}



static int findSynchronizable(vmc_t *container, event_t *evts, cam_event_t *cev){
  heap_index index = *evts;
  do{

    cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);

    cam_value_t base_evt_cam =
      heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

    cam_value_t message =
      heap_snd(&container->heap, (heap_index)cam_evt_pointer.value);


    base_event_t bevt =
      {   .e_type = extract_bits(base_evt_cam.value, 24, sizeof(event_type_t))
        , .channel_id = extract_bits(base_evt_cam.value, 16, sizeof(UUID))
        , .wrap_label = extract_bits(base_evt_cam.value,  0, sizeof(uint16_t))
      };

    cam_event_t cevt = { .bev = bevt, .msg = message };

    if(bevt.e_type == SEND){

      if(poll_recvq(&container->channels[bevt.channel_id].recvq)){
        *cev = cevt;
        return 1;
      } // else continue the do-while loop

    } else { // recvEvt

      if(poll_sendq(&container->channels[bevt.channel_id].sendq)){
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
  heap_index index = *evts;
  do{

    cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);

    cam_value_t base_evt_cam =
      heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

    cam_value_t msg =
      heap_snd(&container->heap, (heap_index)cam_evt_pointer.value);


    base_event_t bevt =
      {   .e_type = extract_bits(base_evt_cam.value, 24, sizeof(event_type_t))
        , .channel_id = extract_bits(base_evt_cam.value, 16, sizeof(UUID))
        , .wrap_label = extract_bits(base_evt_cam.value,  0, sizeof(uint16_t))
      };

    if(bevt.e_type == SEND){

      bool dirty = false;

      //XXX: Instead of copying the whole message send its reference.
      // chan_data_t should have the field cam_value_t *message
      chan_data_t sender_data =
        {   .context_id = container->current_running_context_id
          , .message = msg
          , .dirty_flag = &dirty };

      int j =
        chan_q_enqueue(&container->channels[bevt.channel_id].sendq, sender_data);

      if(j == -1){
        DEBUG_PRINT(( "Cannot enqueue in channel %u 's send queue \n"
                     , bevt.channel_id));
        return -1;
      }

    } else { // recvEvt

      bool dirty = false;

      recv_data_t recv_data =
        {   .context_id = container->current_running_context_id
          , .dirty_flag = &dirty };

      int j =
        chan_recv_q_enqueue(  &container->channels[bevt.channel_id].recvq
                            , recv_data);
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


  base_event_t bevt = cev.bev;
  cam_value_t  message = cev.msg; // NULL for recv


  if(bevt.e_type == SEND){

    recv_data_t recv_data;//recv_context_id;
    int deq_status =
      chan_recv_q_dequeue(&container->channels[bevt.channel_id].recvq, &recv_data);
    if(deq_status == -1){ //empty queue
      DEBUG_PRINT(( "Recv Queue of %u empty for syncing send \n"
                   , bevt.channel_id));
      return -1;
    }

    UUID recv_context_id = recv_data.context_id;
    *recv_data.dirty_flag = true; // the unlogging trick


    /* NOTE Message passing begins */
    /*
     * Put the message residing on the receiving context's env register
     */

    container->contexts[recv_context_id].env = message;

    //XXX: PC_IDX should be moved to bevt.wrap_label here and the
    // wrapped function should be applied now

    /* NOTE Message passing ends */

    int enq_status =
      q_enqueue(&container->rdyQ, container->current_running_context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }

    /****** PC increment *****/

    container->contexts[container->current_running_context_id].pc++; // increments the PC of the sender
    container->contexts[recv_context_id].pc++; // increments the PC of the receiver

    /****** PC increment *****/

    // the receiving thread will run now
    container->current_running_context_id = recv_context_id;

    return 1;

  } else {

    chan_data_t sender_data;
    int deq_status =
      chan_q_dequeue(&container->channels[bevt.channel_id].sendq, &sender_data);

    if(deq_status == -1){ //empty queue
      DEBUG_PRINT((  "Send Queue of %u empty for syncing recv \n"
                   , bevt.channel_id));
      return -1;
    }

    *sender_data.dirty_flag = true; // the unlogging trick

    /* NOTE Message passing begins */
    /*
     * Take the message from sender's environment where it has to be
     * blocked if `sync` call was issued with the message on the env register
     * Place the message on the receivers env (current running context)
     */

    container->contexts[container->current_running_context_id].env =
      sender_data.message;

    //XXX: PC_IDX should be moved to bevt.wrap_label here and the
    // wrapped function should be applied now

    /* NOTE Message passing ends */

    int enq_status = q_enqueue(&container->rdyQ, sender_data.context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }

    /****** PC increment *****/

    container->contexts[sender_data.context_id].pc++; //sender is unblocked now
    container->contexts[container->current_running_context_id].pc++; //continue executing the receiver from sync

    /****** PC increment *****/


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

  UINT data = set_first_16_bits(SEND, *chan_id); // wrap_label not set
  cam_value_t event = {.value = data, .flags = 0};

  heap_index cev_idx = heap_alloc_withGC(container);

  if(cev_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for cam_event_t has failed"));
    return -1;
  }
  heap_set(&container->heap, cev_idx, event, msg);

  cam_value_t heap_cell = {.value = (UINT)cev_idx, .flags = VALUE_PTR_BIT };
  cam_value_t null  = {.value = (UINT)HEAP_NULL, .flags = 0};

  heap_index hi = heap_alloc_withGC(container);

  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for event_t has failed"));
    return -1;
  }

  heap_set(&container->heap, hi, heap_cell, null);

  *sevt = hi;

  return 1;

}

int recvEvt(vmc_t *container, UUID *chan_id, event_t *revt){


  UINT data = set_first_16_bits(RECV, *chan_id); // wrap_label not set
  cam_value_t event = {.value = data, .flags = 0};
  cam_value_t null_msg  = {.value = (UINT)HEAP_NULL, .flags = 0};

  heap_index cev_idx = heap_alloc_withGC(container);

  if(cev_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for cam_event_t has failed"));
    return -1;
  }
  heap_set(&container->heap, cev_idx, event, null_msg);

  cam_value_t heap_cell = {.value = (UINT)cev_idx, .flags = VALUE_PTR_BIT };
  cam_value_t null  = {.value = (UINT)HEAP_NULL, .flags = 0};

  heap_index hi = heap_alloc_withGC(container);

  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for event_t has failed"));
    return -1;
  }

  heap_set(&container->heap, hi, heap_cell, null);

  *revt = hi;

  return 1;

}

