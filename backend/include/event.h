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

#ifndef __EVENT_H_
#define __EVENT_H_

#include <VMC.h>

typedef enum {
   SEND,
   RECV
} event_type_t;

typedef struct {
  event_type_t e_type; //  8 bits
  UUID channel_id;     //  8 bits
} base_evt_simple_t;

typedef struct {
  base_evt_simple_t  evt_details;   // stored in a cam_value_t with 16 bits free
  cam_value_t wrap_func_ptr; // 32 bits
} base_event_t;


typedef struct {
  base_event_t bev; // 32 bits
  cam_value_t  msg; // 32 bits; NULL for recv
} cam_event_t;

typedef heap_index event_t;

extern bool poll_sendq(vmc_t *container, chan_send_queue_t *q);
extern bool poll_recvq(vmc_t *container, chan_recv_queue_t *q);

/*
 *  Proposed heap structure
 *  base_event_t -> fst = top    16 bits free;
 *                        bottom 16 bits base_evt_simple_t
 *               -> snd = pointer to wrap_func - [v:l] or [l]
 *
 *  cam_event_t -> fst = pointer to base_event_t
 *              -> snd = message or pointer to message or null for recv
 *
 *  heap_cell_list -> fst = pointer to cam_event_t
 *                 -> snd = pointer to next heap_cell_ev
 */

#endif
