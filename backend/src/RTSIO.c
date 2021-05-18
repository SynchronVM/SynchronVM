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
  /*
   * Put the message residing on the receiving context's env register
   */

  vmc->contexts[recv_context_id].env = msg;

  //XXX: PC_IDX should be moved to bevt.wrap_label here and the
  // wrapped function should be applied now

  /* NOTE Message passing ends */


  // the receiving thread will run now
  vmc->current_running_context_id = recv_context_id;

  return 1;

}
