/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Abhiroop Sarkar             				  */
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

#include<priorityqueue.h>
#include<stdbool.h>


static inline void swapElems(pq_data_t *x, pq_data_t *y)
{
  pq_data_t temp = *x;
  *x = *y;
  *y = temp;
}

static inline int parentIdx(int i) { return (i-1)/2; }

static inline int leftIdx(int i) { return (2*i + 1); }

static inline int rightIdx(int i) { return (2*i + 2); }



static void minHeapify(PriorityQ_t *pq, int i)
{
  int l = leftIdx(i);
  int r = rightIdx(i);
  int smallest = i;
  if (l < pq->size && pq->data[l].ticks < pq->data[i].ticks)
    smallest = l;
  if (r < pq->size && pq->data[r].ticks < pq->data[smallest].ticks)
    smallest = r;
  if (smallest != i)
    {
      swapElems(&pq->data[i], &pq->data[smallest]);
      minHeapify(pq, smallest);
    }
}

int pq_init(PriorityQ_t *pq, uint8_t *mem, unsigned int size_bytes){

  if (!mem || !pq || size_bytes < sizeof(pq_data_t)) return 0;
  unsigned int num_elt = size_bytes / sizeof(pq_data_t);
  pq->capacity = num_elt;
  pq->size = 0;
  pq->data  = (pq_data_t*)mem;

  return 1;

}

int pq_insert(PriorityQ_t *pq, pq_data_t pq_data){

  if(pq->size == pq->capacity){
    DEBUG_PRINT(("Priority queue full; Cannot insert\n"));
    return -1;
  }

  pq->size++;
  int insertIdx = pq->size - 1;
  pq->data[insertIdx] = pq_data;

  while(insertIdx != 0 &&
        (pq->data[parentIdx(insertIdx)].ticks > pq->data[insertIdx].ticks)){
    swapElems(&pq->data[insertIdx], &pq->data[parentIdx(insertIdx)]);
    insertIdx = parentIdx(insertIdx);
  }
  return 1;
}

int pq_extractMin(PriorityQ_t *pq, pq_data_t *pq_data){

  if(pq->size <= 0){
    DEBUG_PRINT(("Priority queue is empty\n"));
    return -1;
  }

  if(pq->size == 1){
    pq->size--;
    *pq_data = pq->data[0];
    return 1;
  }

  *pq_data = pq->data[0]; // the root is returned

  // heapify the remaining tree
  pq->data[0] = pq->data[pq->size - 1];
  pq->size--;
  minHeapify(pq, 0);

  return 1;
}
int pq_getMin(PriorityQ_t *pq, pq_data_t *pq_data){

  if(pq_isEmpty(pq)){
    DEBUG_PRINT(("Priority queue is empty\n"));
    return -1;
  }

  *pq_data = pq->data[0];
  return 1;
}

bool pq_isEmpty(PriorityQ_t *pq) { return (pq->size == 0); }
