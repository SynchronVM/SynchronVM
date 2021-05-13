/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar             				  */
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

#include<chan_send_queue.h>

int chan_send_q_init(chan_send_queue_t *q, uint8_t *mem, unsigned int size_bytes){

  if (!mem || !q || size_bytes < sizeof(send_data_t)) return 0;
  unsigned int num_elt = size_bytes / sizeof(send_data_t);
  q->capacity = num_elt;
  q->front = q->size = 0;
  q->rear  = num_elt - 1;
  q->data  = (send_data_t*)mem;

  return 1;

}


static inline bool is_full(chan_send_queue_t *q)
{  return (q->size == q->capacity);  }

static inline bool is_empty(chan_send_queue_t *q)
{  return (q->size == 0); }

int chan_send_q_enqueue(chan_send_queue_t *q, send_data_t send_data){

  if (is_full(q)){
    DEBUG_PRINT(("Chan recv queue is full\n"));
    return -1;
  }
  q->rear = (q->rear + 1)%q->capacity;
  q->data[q->rear] = send_data;
  q->size = q->size + 1;
  return 1;

}

int chan_send_q_dequeue(chan_send_queue_t *q, send_data_t *send_data){

  if (is_empty(q)){
    DEBUG_PRINT(("Chan send queue is empty\n"));
    return -1;
  }
  *send_data = q->data[q->front];
  q->front = (q->front + 1)%q->capacity;
  q->size = q->size - 1;
  return 1;
}

int chan_send_q_front  (chan_send_queue_t *q, send_data_t *send_data){

  if (is_empty(q)){
    DEBUG_PRINT(("Chan send queue is empty\n"));
    return -1;
  }

  *send_data = q->data[q->front];
  return 1;
}
