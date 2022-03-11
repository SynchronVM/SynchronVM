/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar 				  */
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

#ifndef SYS_TIME_H_
#define SYS_TIME_H_

/* sys is at a slightly lower level in the "tree of abstractions"
   as ll_driver may come to depend upon these things. */

/* Each port must implement these operations */

#include <stdint.h>
#include <stdbool.h>

#include <typedefs.h> /* for definition of Time type */

#define SYS_TIME_SENDER_ID 255

/* initialize the timers, takes an os_interop pointer
   to enable sending of messages to the scheduler message queue
*/
extern bool      sys_time_init(void *os_interop);

extern Time      sys_time_get_current_ticks(void);
extern uint32_t  sys_time_get_alarm_channels(void);
extern uint32_t  sys_time_get_clock_freq(void);

/* Sends a timestamped message to the scheduler message queue at an absolute time */
extern bool      sys_time_set_wake_up(Time absolute);

/* Return the wake up time of the current alarm */
extern Time      sys_get_wake_up_time(void);

extern bool      sys_is_alarm_set(void);

/* put OS thread to sleep, risky operation */
extern void      sys_sleep_ms(uint32_t ms);

extern uint32_t sys_get_timestamp(void);

#endif
