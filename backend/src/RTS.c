
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

#define SYNC_DRIVER 7

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


static inline UINT set_bottom_16_bits(uint8_t first8bits, uint8_t second8bits){

  UINT value = 0;
  value = value | (first8bits << 8);
  value = value | second8bits;

  return value;

}

static cam_value_t create_dirty_flag(vmc_t *container, bool b){

  cam_value_t dirty_flag;

  if(b)
    dirty_flag = (cam_value_t){ .value = 1, .flags = 0 }; // true
  else
    dirty_flag = (cam_value_t){ .value = 0, .flags = 0 }; // false

  heap_index dirty_flag_idx = heap_alloc_withGC(container);
  heap_set_fst(&container->heap, dirty_flag_idx, dirty_flag);
  cam_value_t dirty_flag_pointer =
    { .value = (UINT)dirty_flag_idx, .flags = VALUE_PTR_BIT };

  return dirty_flag_pointer;

}


static int findSynchronizable(vmc_t *container, event_t *evts, cam_event_t *cev){
  heap_index index = *evts;
  do{

    cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);



    cam_value_t base_evt_ptr =
      heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

    cam_value_t message =
      heap_snd(&container->heap, (heap_index)cam_evt_pointer.value);



    cam_value_t base_evt_simple =
      heap_fst(&container->heap, (heap_index)base_evt_ptr.value);

    cam_value_t wrap_fptr =
      heap_snd(&container->heap, (heap_index)base_evt_ptr.value);



    base_evt_simple_t bevt_simple =
      {   .e_type     = extract_bits(base_evt_simple.value,  8, 8)
        , .channel_id = extract_bits(base_evt_simple.value,  0, 8)
      };

    base_event_t bevt = {   .evt_details   = bevt_simple
                          , .wrap_func_ptr = wrap_fptr };

    cam_event_t cevt = { .bev = bevt, .msg = message };

    /**** Dealing with synchronous drivers like LEDs *****/
    if(container->channels[bevt_simple.channel_id].sync_driver_no != DRIVER_NULL){
      // should use ll_data_readable/ ll_data_writeable depending on the event_type here
      *cev = cevt;
      return SYNC_DRIVER;
    }
    /****************************************************/

    if(bevt_simple.e_type == SEND){

      if(poll_recvq(container, &container->channels[bevt_simple.channel_id].recvq)){
        *cev = cevt;
        return 1;
      } // else continue the do-while loop

    } else if (bevt_simple.e_type == RECV) { // recvEvt

      if(poll_sendq(container, &container->channels[bevt_simple.channel_id].sendq)){
        *cev = cevt;
        return 1;
      }

    }

    // else continue the do-while loop

    cam_value_t pointer_to_next = heap_snd(&container->heap, index);
    index = (heap_index)pointer_to_next.value;

  } while(index != HEAP_NULL);

  return -1;
}

static int blockAllEvents(vmc_t *container, event_t *evts){
  heap_index index = *evts;
  do{

    cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);



    cam_value_t base_evt_ptr =
      heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

    cam_value_t msg =
      heap_snd(&container->heap, (heap_index)cam_evt_pointer.value);



    cam_value_t base_evt_simple =
      heap_fst(&container->heap, (heap_index)base_evt_ptr.value);

    cam_value_t wrap_fptr =
      heap_snd(&container->heap, (heap_index)base_evt_ptr.value);



    base_evt_simple_t bevt_simple =
      {   .e_type     = extract_bits(base_evt_simple.value,  8, 8)
        , .channel_id = extract_bits(base_evt_simple.value,  0, 8)
      };


    base_event_t bevt =
      {   .evt_details   = bevt_simple
        , .wrap_func_ptr = wrap_fptr
      };

    (void)bevt;// unused

    if(bevt_simple.e_type == SEND){

      //Create dirty flag = false
      cam_value_t df_pointer = create_dirty_flag(container, false);



      //XXX: Instead of copying the whole message send its reference.
      // send_data_t should have the field cam_value_t *message
      send_data_t sender_data =
        {   .context_id = container->current_running_context_id
          , .message = msg
          , .dirty_flag_pointer = df_pointer };

      int j =
        chan_send_q_enqueue(&container->channels[bevt_simple.channel_id].sendq, sender_data);

      if(j == -1){
        DEBUG_PRINT(( "Cannot enqueue in channel %u 's send queue \n"
                     , bevt_simple.channel_id));
        return -1;
      }

    } else if (bevt_simple.e_type == RECV){ // recvEvt

      // create dirty flag = false
      cam_value_t df_pointer = create_dirty_flag(container, false);

      recv_data_t recv_data =
        {   .context_id = container->current_running_context_id
          , .dirty_flag_pointer = df_pointer };

      int j =
        chan_recv_q_enqueue(  &container->channels[bevt_simple.channel_id].recvq
                            , recv_data);
      if(j == -1){
        DEBUG_PRINT((" Cannot enqueue in channel %u 's recv queue \n"
                     , bevt_simple.channel_id));
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

      /*** Push label graveyard address on the stack ****/
      INT jump_address = container->code_size - 1;
      cam_value_t j_add = { .value = (UINT)jump_address };
      int q = stack_push(&container->contexts[i].stack, j_add);
      if(q == 0){
        DEBUG_PRINT(("Stack push has failed"));
        return -1;
      }
      /**************************************************/

      container->context_used[i] = true;
      int j = q_enqueue(&container->rdyQ, i);
      if(j == -1){
        DEBUG_PRINT(("Cannot enqueue in ready queue \n"));
        return -1;
      }

      // Place the context-id(or process-id) on the environment register
      cam_value_t process_id = { .value = (UINT)i, .flags = 0 };
      container->contexts[container->current_running_context_id].env = process_id;

      // eval_RTS_spawn should now simply do *pc_idx+=2
      // so that the parent context can continue running
      return 1;
    }
  }
  DEBUG_PRINT(("Cannot spawn more threads \n"));
  return -1;
}

int dispatch(vmc_t *container){
  UUID context_id;
  int de_q_status = q_dequeue(&container->rdyQ, &context_id);
  if (de_q_status == -1){
    // This is the standard state of a microcontroller
    // where processes are blocked and sleeping, waiting
    // for interrupts to arrive. Setting the
    // current_running_context_id = UUID_NONE is an indicator
    // to zephyr to now wait for interrupts;
    DEBUG_PRINT(("Ready Queue is empty\n"));
    container->current_running_context_id = UUID_NONE;
    return -1;
  }
  DEBUG_PRINT(("Queueing\n"));
  container->current_running_context_id = context_id;
  return 1;
}

static int postSync( vmc_t *container
                   , cam_value_t wrap_fptr
                   , cam_value_t msg_content
                   , UUID ctx_id){


  heap_index closure_address = wrap_fptr.value;

  cam_value_t heap_f = heap_fst(&container->heap, closure_address);
  cam_value_t heap_s = heap_snd(&container->heap, closure_address);

  cam_value_t label;


  //update the env
  if(heap_s.value == 4294967295){ // if combinator

    label = heap_f;

    container->contexts[ctx_id].env = msg_content;

  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    label = heap_s;

    heap_index hi = heap_alloc_withGC(container);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation failed in post-syncer"));
      return -1;
    }
    heap_set(&container->heap, hi, val, msg_content);
    cam_value_t new_env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    container->contexts[ctx_id].env
      = new_env_pointer;
  }


  // update the PC
  UINT current_pc =
    container->contexts[ctx_id].pc;
  cam_value_t j_add = { .value = current_pc };
  int q =
    stack_push(  &container->contexts[ctx_id].stack
               , j_add);
  if(q == 0){
    DEBUG_PRINT(("Stack push failed in post-syncer"));
    return -1;
  }

  container->contexts[ctx_id].pc = label.value;

  return 1;

}

static int message_pass( vmc_t *container
                       , UUID ctx_id
                       , cam_value_t msg
                       , UUID chan_id
                       , event_type_t ety){
  //This function looks at the environment of the blocked context;
  // It finds the event which demanded synchronization and checks
  // if there are post synchronization actions associated with it
  // If yes; it calls postSync otherwise simply places the msg on the env
  cam_value_t event = container->contexts[ctx_id].env;
  heap_index index  = event.value;
  do{
    cam_value_t cam_evt_pointer = heap_fst(&container->heap, index);

    cam_value_t base_evt_ptr =
      heap_fst(&container->heap, (heap_index)cam_evt_pointer.value);

    cam_value_t base_evt_simple =
      heap_fst(&container->heap, (heap_index)base_evt_ptr.value);

    cam_value_t wrap_fptr =
      heap_snd(&container->heap, (heap_index)base_evt_ptr.value);

    base_evt_simple_t bevt_simple =
      {   .e_type     = extract_bits(base_evt_simple.value,  8, 8)
        , .channel_id = extract_bits(base_evt_simple.value,  0, 8)
      };

    if((bevt_simple.e_type == ety) && (bevt_simple.channel_id == chan_id)){
      if((heap_index)wrap_fptr.value != HEAP_NULL){

        int q = postSync( container
                        , wrap_fptr
                        , msg
                        , ctx_id);
        if(q == -1){
          DEBUG_PRINT(("Post synchronization error\n"));
          return q;
        }
        return 1;

      } else {
        container->contexts[ctx_id].env = msg;
        return 1;
      }
    }
    cam_value_t pointer_to_next = heap_snd(&container->heap, index);
    index = (heap_index)pointer_to_next.value;


  } while(index != HEAP_NULL);

  return -1; // could not find the right event to sync
}

static int synchronizeNow(vmc_t *container, cam_event_t cev){
  /* NOTE: BEWARE! SEND and RECV have different behaviours! Study */
  /* both the if and else blocks carefully to understand the */
  /* difference. In both cases the receiving thread starts executing */
  /* when `sync` succeeds! Therefore the code is differnt if you */
  /* view it from the perspective of the sender or the receiver. */

  base_event_t bevt = cev.bev;
  cam_value_t  message = cev.msg; // NULL for recv

  base_evt_simple_t bevt_simple = bevt.evt_details;
  cam_value_t wrap_fptr = bevt.wrap_func_ptr;


  if(bevt_simple.e_type == SEND){

    recv_data_t recv_data;//recv_context_id;
    int deq_status =
      chan_recv_q_dequeue(&container->channels[bevt_simple.channel_id].recvq, &recv_data);
    if(deq_status == -1){ //empty queue
      DEBUG_PRINT(( "Recv Queue of %u empty for syncing send \n"
                   , bevt_simple.channel_id));
      return -1;
    }

    UUID recv_context_id = recv_data.context_id;

    cam_value_t true_flag = { .value = 1, .flags = 0 };
    heap_set_fst(  &container->heap
                 , (heap_index)recv_data.dirty_flag_pointer.value
                 , true_flag); //the unlogging trick



    /* NOTE Message passing begins */
    /*
     */


    int k = message_pass( container
                        , recv_context_id
                        , message
                        , bevt_simple.channel_id
                        , RECV);
    if(k == -1){
      DEBUG_PRINT(("Error in message passing"));
      return -1;
    }


    /* NOTE Message passing ends */

    int enq_status =
      q_enqueue(&container->rdyQ, container->current_running_context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }

    /* NOTE Post synchronization actions begins */
    // place the () on sender's env because sync (send) succeeded

    cam_value_t empty_tuple = { .value = 0, .flags = 0 };

    if((heap_index)wrap_fptr.value != HEAP_NULL){

      int q = postSync( container
                      , wrap_fptr
                      , empty_tuple
                      , container->current_running_context_id);
      if(q == -1){
        DEBUG_PRINT(("Post synchronization error\n"));
        return q;
      }


    } else {
      container->contexts[container->current_running_context_id].env
        = empty_tuple;
    }
    /* NOTE Post synchronization actions ends */


    // the receiving thread will run now
    container->current_running_context_id = recv_context_id;

    return 1;

  } else if(bevt_simple.e_type == RECV) {

    send_data_t sender_data;
    int deq_status =
      chan_send_q_dequeue(&container->channels[bevt_simple.channel_id].sendq, &sender_data);

    if(deq_status == -1){ //empty queue
      DEBUG_PRINT((  "Send Queue of %u empty for syncing recv \n"
                   , bevt_simple.channel_id));
      return -1;
    }

    cam_value_t true_flag = { .value = 1, .flags = 0 };
    heap_set_fst(  &container->heap
                 , (heap_index)sender_data.dirty_flag_pointer.value
                 , true_flag); //the unlogging trick


    /* NOTE Message passing begins */


    if((heap_index)wrap_fptr.value != HEAP_NULL){

      int q = postSync( container
                      , wrap_fptr
                      , sender_data.message
                      , container->current_running_context_id);
      if(q == -1){
        DEBUG_PRINT(("Post synchronization error\n"));
        return q;
      }


    } else {
      container->contexts[container->current_running_context_id].env
        = sender_data.message;
    }



    /* NOTE Message passing ends */

    int enq_status = q_enqueue(&container->rdyQ, sender_data.context_id); // queueing sender
    if (enq_status == -1){
      DEBUG_PRINT(("Ready Queue is full\n"));
      return -1;
    }


    cam_value_t empty_tuple = { .value = 0, .flags = 0 };
    /* container->contexts[sender_data.context_id].env = empty_tuple; */
    int k = message_pass( container
                        , sender_data.context_id
                        , empty_tuple
                        , bevt_simple.channel_id
                        , SEND);
    if(k == -1){
      DEBUG_PRINT(("Error in message passing"));
      return -1;
    }


    // the receiver will automatically run because we are now executing its context
    return 1;

  }

  return -1; // neither SEND or RECV
}

static int synchronizeSyncDriver(vmc_t *container, cam_event_t cev);

int sync(vmc_t *container, event_t *evts){
  cam_event_t cev;
  int i = findSynchronizable(container, evts, &cev);

  if(i == 1){

    int sync_status = synchronizeNow(container, cev);
    if(sync_status == -1){
      DEBUG_PRINT(("Synchronization failed! \n"));
      return -1;
    }

  } else if(i == SYNC_DRIVER){
    int sync_status = synchronizeSyncDriver(container, cev);
    if(sync_status == -1){
      DEBUG_PRINT(("Synchronization on sync driver failed! \n"));
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

  cam_value_t null  = {.value = (UINT)HEAP_NULL, .flags = 0};


  /*
   *  base_event_t -> fst = top    16 bits free;
   *                        bottom 16 bits base_evt_simple_t
   *               -> snd = pointer to wrap_func - [v:l] or [l]
   *
   */

  UINT data = set_bottom_16_bits(SEND, *chan_id);

  cam_value_t base_evt_simple = { .value = data, .flags = 0};

  heap_index base_evt_idx = heap_alloc_withGC(container);

  if(base_evt_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for base_event_t has failed"));
    return -1;
  }

  heap_set(&container->heap, base_evt_idx, base_evt_simple, null); // pointer to wrap func NULL


  /*
   *  cam_event_t -> fst = pointer to base_event_t
   *              -> snd = message or pointer to message
   */

  cam_value_t event = {.value = (UINT)base_evt_idx, .flags = VALUE_PTR_BIT};

  heap_index cev_idx = heap_alloc_withGC(container);

  if(cev_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for cam_event_t has failed"));
    return -1;
  }
  heap_set(&container->heap, cev_idx, event, msg);


  /*
   *  heap_cell_list -> fst = pointer to cam_event_t
   *                 -> snd = pointer to next heap_cell_ev
   */

  cam_value_t heap_cell = {.value = (UINT)cev_idx, .flags = VALUE_PTR_BIT };

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

  cam_value_t null  = {.value = (UINT)HEAP_NULL, .flags = 0};


  /*
   *  base_event_t -> fst = top    16 bits free;
   *                        bottom 16 bits base_evt_simple_t
   *               -> snd = pointer to wrap_func - [v:l] or [l]
   *
   */

  UINT data = set_bottom_16_bits(RECV, *chan_id);

  cam_value_t base_evt_simple = { .value = data, .flags = 0};

  heap_index base_evt_idx = heap_alloc_withGC(container);

  if(base_evt_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for base_event_t has failed"));
    return -1;
  }

  heap_set(&container->heap, base_evt_idx, base_evt_simple, null); // pointer to wrap func NULL



  /*
   *  cam_event_t -> fst = pointer to base_event_t
   *              -> snd = null for recv
   */


  cam_value_t event = {.value = (UINT)base_evt_idx, .flags = VALUE_PTR_BIT};

  cam_value_t null_msg  = {.value = (UINT)HEAP_NULL, .flags = 0};

  heap_index cev_idx = heap_alloc_withGC(container);

  if(cev_idx == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for cam_event_t has failed"));
    return -1;
  }
  heap_set(&container->heap, cev_idx, event, null_msg);


  /*
   *  heap_cell_list -> fst = pointer to cam_event_t
   *                 -> snd = pointer to next heap_cell_ev
   */

  cam_value_t heap_cell = {.value = (UINT)cev_idx, .flags = VALUE_PTR_BIT };

  heap_index hi = heap_alloc_withGC(container);

  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation for event_t has failed"));
    return -1;
  }

  heap_set(&container->heap, hi, heap_cell, null);

  *revt = hi;

  return 1;

}

int choose (vmc_t *container, event_t *evt1, event_t *evt2, event_t *evts){
  heap_index index1 = *evt1;

  heap_index original_e1_idx = index1;

  while(true){

    /* cam_value_t cam_evt_pointer = heap_fst(&container->heap, index1); */

    cam_value_t pointer_to_next = heap_snd(&container->heap, index1);
    if((heap_index)pointer_to_next.value == HEAP_NULL){
      //reached the end of evt1 list
      break;
    }
    index1 = (heap_index)pointer_to_next.value;

  }

  heap_index index2 = *evt2;
  cam_value_t e2_cam = { .value = (UINT)index2, .flags = VALUE_PTR_BIT };
  heap_set_snd(&container->heap, index1, e2_cam);

  *evts = original_e1_idx;

  return 1;
}


/********** IO operation **********/
static int synchronizeSyncDriver(vmc_t *container, cam_event_t cev){
  base_event_t bevt = cev.bev;
  cam_value_t  message = cev.msg; // NULL for recv

  base_evt_simple_t bevt_simple = bevt.evt_details;
  cam_value_t wrap_fptr = bevt.wrap_func_ptr;


  cam_value_t val_before_post_sync;
  if(bevt_simple.e_type == SEND){

    //XXX: Assuming the message is a simple value and not a pointer for
    //     now; We should make such checks here and serialize accordingly

    uint8_t data_arr[4];
    data_arr[0] = extract_bits(message.value, 24, 8);
    data_arr[1] = extract_bits(message.value, 16, 8);
    data_arr[2] = extract_bits(message.value,  8, 8);
    data_arr[3] = extract_bits(message.value,  0, 8);

    UUID sync_driver_number =
      container->channels[bevt_simple.channel_id].sync_driver_no;

    int k =
      ll_write(&container->drivers[sync_driver_number], data_arr, 1); //writing 1 byte
    if(k != 1){
      DEBUG_PRINT(("Failed to write to sync driver!"));
      return -1;
    }

    cam_value_t empty_tuple = { .value = 0, .flags = 0 };

    val_before_post_sync = empty_tuple;



  } else if(bevt_simple.e_type == RECV) {
    UUID sync_driver_number =
      container->channels[bevt_simple.channel_id].sync_driver_no;
    uint8_t data_arr[4];
    int k =
      ll_read(&container->drivers[sync_driver_number], data_arr, 1); //reading 1 byte
    if(k != 1){
      DEBUG_PRINT(("Failed to read from sync driver!"));
      return -1;
    }

    UINT data = 0;
    data = data | (data_arr[0] << 24);
    data = data | (data_arr[1] << 16);
    data = data | (data_arr[2] <<  8);
    data = data | data_arr[3];

    cam_value_t msg_ = { .value = data, .flags = 0};

    val_before_post_sync = msg_;

  }


  //Post synchronization
  if((heap_index)wrap_fptr.value != HEAP_NULL){

    int q = postSync( container
                    , wrap_fptr
                    , val_before_post_sync
                    , container->current_running_context_id);
    if(q == -1){
      DEBUG_PRINT(("Post synchronization error\n"));
      return q;
    }


  } else {
    container->contexts[container->current_running_context_id].env
      = val_before_post_sync;
  }


  return 1;
}
