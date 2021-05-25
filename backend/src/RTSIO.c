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


#include <RTSIO.h>


// This function is a unchanged copy of the message_pass function
// in RTS.c but because of the strange dependency order of VMC and
// RTS cannot reside there; Perhaps the scheduler should be in a
// different file
static int message_pass(  vmc_t *container
                        , UUID ctx_id
                        , cam_value_t msg
                        , UUID chan_id
                        , event_type_t ety);



int handle_msg(vmc_t *vmc, ll_driver_msg_t *m){

  UUID chan_id = vmc->drivers[m->driver_id].channel_id;
  //XXX: Logic here to decide if the arrived message is
  //     the entire message or the pointer to a location to read
  cam_value_t msg = { .value = m->data, .flags = 0 };



  // The following code is a barebones version of what `sync` does

  recv_data_t recv_data;//recv_context_id;
  int deq_status =
    chan_recv_q_dequeue(&vmc->channels[chan_id].recvq, &recv_data);
  if(deq_status == -1){ //empty queue
    DEBUG_PRINT(( "Recv Queue of %u empty for sending interrupt \n"
                  , chan_id));
    //XXX: Need to do something here
    return -1;
  }

  UUID recv_context_id = recv_data.context_id;

  cam_value_t true_flag = { .value = 1, .flags = 0 };
  heap_set_fst(  &vmc->heap
		 , (heap_index)recv_data.dirty_flag_pointer.value
		 , true_flag); //the unlogging trick
  


  /* NOTE Message passing begins */
  
  int k = message_pass( vmc
			, recv_context_id
			, msg
			, chan_id
			, RECV);
  if(k == -1){
    DEBUG_PRINT(("Error in message passing"));
    return -1;
  }

  /* NOTE Message passing ends */


  // the receiving thread will run now
  vmc->current_running_context_id = recv_context_id;

  return 1;

}





// XXX: All these functions are copied unchanged from RTS.c

static inline UINT extract_bits(UINT value, int lsbstart, int numbits){
  UINT mask = (1 << numbits) - 1;
  return ( mask & (value >> lsbstart));
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

    heap_index hi = vmc_heap_alloc_withGC(container);
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
