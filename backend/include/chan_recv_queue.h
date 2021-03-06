/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar             		  */
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

#ifndef __CHAN_RECV_QUEUE_H_
#define __CHAN_RECV_QUEUE_H_

#include <stdbool.h>
#include <typedefs.h>

typedef struct {
  UUID  context_id;
  cam_value_t dirty_flag_pointer;
} recv_data_t;


typedef struct {
  recv_data_t *data;
  int capacity;
  int front;
  int rear;
  int size;
} chan_recv_queue_t;


extern int chan_recv_q_init(chan_recv_queue_t *q, uint8_t *mem, unsigned int size_bytes);

extern int chan_recv_q_enqueue(chan_recv_queue_t *q, recv_data_t chan_data);
extern int chan_recv_q_dequeue(chan_recv_queue_t *q, recv_data_t *chan_data);
extern int chan_recv_q_front  (chan_recv_queue_t *q, recv_data_t *chan_data);



#endif
