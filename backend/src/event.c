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

#include <SVM_DEBUG.h>

#include <event.h>

bool poll_sendq(vmc_t *container, chan_send_queue_t *q){
  while(true){
    send_data_t send_data;
    int op_status = chan_send_q_front(q, &send_data);
    if(op_status == -1){ //empty queue
      return false;
    } else {
      cam_value_t dirty_flag =
        heap_fst(  &container->heap
                 , (heap_index)send_data.dirty_flag_pointer.value);
      if((dirty_flag.value & 1) == 1){ // if dirty flag is SET
        send_data_t temp;
        chan_send_q_dequeue(q, &temp); // no need to check status we know there is data
      } else {
        return true; // the actual dequeing should happen inside doFn
      }
    }
  }
}

bool poll_recvq(vmc_t *container, chan_recv_queue_t *q){
  while(true){
    recv_data_t recv_data;
    int op_status = chan_recv_q_front(q, &recv_data);
    if(op_status == -1){ //empty queue
      return false;
    } else {
      cam_value_t dirty_flag =
        heap_fst(  &container->heap
                 , (heap_index)recv_data.dirty_flag_pointer.value);

      if((dirty_flag.value & 1) == 1){ // if dirty flag is SET
        recv_data_t temp;
        chan_recv_q_dequeue(q, &temp); // no need to check status
                                       // we know there is data
      } else {
        return true; // the actual dequeing should happen inside doFn
      }
    }
  }
}
