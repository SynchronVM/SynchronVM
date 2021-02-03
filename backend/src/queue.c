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
#include<stdbool.h>

int q_init(Queue_t *q, uint8_t *mem, unsigned int size_bytes){

  if (!mem || !q || size_bytes < 256) return 0;
  unsigned int num_elt = size_bytes / sizeof(UUID);
  q->capacity = num_elt;
  q->front = q->size = 0;
  q->rear  = num_elt - 1;
  q->data  = mem;

  return 1;
}

static inline bool is_full(Queue_t *q)
{  return (q->size == q->capacity);  }

static inline bool is_empty(Queue_t *q)
{  return (q->size == 0); }

int q_enqueue(Queue_t *q, UUID context_id){


  if (is_full(q)){
      DEBUG_PRINT(("Queue is full\n"));
      return -1;
  }
  q->rear = (q->rear + 1)%q->capacity;
  q->data[q->rear] = context_id;
  q->size = q->size + 1;
  return 1;
}

int q_dequeue(Queue_t *q, UUID *context_id){


  if (is_empty(q)){
      DEBUG_PRINT(("Queue is empty\n"));
      return -1;
  }
  *context_id = q->data[q->front];
  q->front = (q->front + 1)%q->capacity;
  q->size = q->size - 1;
  return 1;
}

int q_front(Queue_t *q, UUID *context_id){


  if (is_empty(q)){
    DEBUG_PRINT(("Queue is empty\n"));
    return -1;
  }

  *context_id = q->data[q->front];
  return 1;
}


static inline void remove_pos_linear(Queue_t *q, int pos){
  for(int i = pos; i < q->rear; i++){
    q->data[i] = q->data[i+1];
  }
  q->rear--;
}

static inline void remove_pos_circular(Queue_t *q, int pos){
  for(int i = pos; i <= q->capacity-2; i++){
    q->data[i] = q->data[i+1];
  }

  q->data[q->capacity-1] = q->data[0];
  remove_pos_linear(q,0);

  if(q->rear == -1)
    q->rear = q->capacity-1;

}

static inline void remove_from_q(  UUID *context_id
                                   , Queue_t *q
                                   , int start
                                   , int end){

  for(int i = start; i <= end; i++){
    while(q->data[i] == *context_id && i <= end){
      remove_pos_linear(q,i);
      end--; //end is q->rear which will decrement after one remove
    }
  }
}



int q_remove(Queue_t *q, UUID *context_id){

  if (is_empty(q)){
    return 1; // queue is empty nothing to remove
  }


  if(q->front <= q->rear){ // queue hasn't circled yet

    remove_from_q(context_id, q, q->front, q->rear);
    return 1;

  } else { // queue has circled already

    /* complex case with a circular queue*/
    /* 2 regions: */
    /* Region 1 - 0 to q->rear  */
    /* Region 2 - q->front to q->capacity-1*/

    /** Region 1 **/
    remove_from_q(context_id, q, 0, q->rear);

    if(q->rear == -1)
      q->rear = q->capacity-1;


    /** Region 2 **/

    for(int i = q->front; i <= q->capacity - 1; i++){

      while(q->data[i] == *context_id && i <= (q->capacity - 1)){

        if(q->front <= q->rear){
          remove_from_q(context_id, q, i, q->rear);
          return 1;
        }

        remove_pos_circular(q,i);
      }

    }
    return 1;
  }
}
