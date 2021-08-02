/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2022 Joel Svensson, Abhiroop Sarkar 				  */
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

#ifndef LL_SYS_TIME_H_
#define LL_SYS_TIME_H_

/* ll_sys is at a slightly lower level in the "tree of abstractions"
   as ll_driver may come to depend upon these things. */

/* Each port must implement these operations */

#include <stdint.h>
#include <stdbool.h>

typedef struct {
  uint32_t high_word;
  uint32_t low_word;
} ll_sys_time_t;

/* initialize the timers, takes an os_interop pointer 
   to enable sending of messages to the scheduler message queue
*/
extern bool sys_time_init(void *os_interop);

extern ll_sys_time_t sys_time_get_current_ticks(void);
extern uint32_t      sys_time_get_clock_freq(void);

/* Sends a timestamped message to the scheduler message queue at an absolute time */
extern bool          sys_time_set_wake_up(ll_sys_time_t absolute);

/* put OS thread to sleep, risky operation */
extern void          sys_sleep_ms(uint32_t ms); 

#endif
