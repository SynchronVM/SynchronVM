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

#include<queue.h>

int chan_q_init(chan_queue_t *q, uint8_t *mem, unsigned int size_bytes){

  if (!mem || !q || size_bytes < 256) return 0;
  unsigned int num_elt = size_bytes / sizeof(UUID);
  q->front = -1;
  q->rear  = -1;
  q->data  = mem;
  q->capacity = num_elt;
  return 1;
}

int chan_q_enqueue(chan_queue_t *q, UUID context_id){
  if ((q->front == 0 && q->rear == q->capacity-1) || (q->rear == (q->front-1)%(q->capacity-1))){
    DEBUG_PRINT(("Queue is full\n"));
    return -1;
  }

  else if (q->front == -1){ /* Insert First Element */
    q->front = 0;
    q->rear = 0;
    q->data[q->rear] = context_id;
  }
  else if (q->rear == q->capacity-1 && q->front != 0) { /* Circle around */
    q->rear = 0;
    q->data[q->rear] = context_id;
  }
  else {
    q->data[q->rear++] = context_id;
  }
  return 1;
}

int chan_q_dequeue(chan_queue_t *q, UUID *context_id){
  if (q->front == -1){
    DEBUG_PRINT(("Queue is empty\n"));
    return -1;
  }

  *context_id = q->data[q->front];
  q->data[q->front] = -1;
  if (q->front == q->rear){
    q->front = -1;
    q->rear = -1;
  }
  else if (q->front == q->capacity-1)
    q->front = 0;
  else
    q->front++;
  return 1;
}

int chan_q_front(chan_queue_t *q, UUID *context_id){
  if (q->front == -1){
    DEBUG_PRINT(("Queue is empty\n"));
    return -1;
  }

  *context_id = q->data[q->front];
  return 1;
}
