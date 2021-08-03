/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Abhiroop Sarkar             		  */
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

#ifndef __PRIORITYQUEUE_H_
#define __PRIORITYQUEUE_H_

#include <stdbool.h>
#include <typedefs.h>

typedef struct {
  UUID  context_id;
  Time  ticks; // baseline for waitQ; deadline for rdyQ
} pq_data_t;



typedef struct {
  pq_data_t *data;
  int capacity;
  int size;
} PriorityQ_t;

extern int  pq_init(PriorityQ_t *pq, uint8_t *mem, unsigned int size_bytes);

extern int  pq_insert    (PriorityQ_t *pq, pq_data_t  pq_data);
extern int  pq_extractMin(PriorityQ_t *pq, pq_data_t *pq_data);
extern int  pq_getMin    (PriorityQ_t *pq, pq_data_t *pq_data);
extern bool pq_isEmpty   (PriorityQ_t *pq);


#endif
