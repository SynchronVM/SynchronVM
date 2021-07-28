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

#include <zephyr.h>

#include <ll/ll_sys_time.h>
#include <hal/zephyr/svm_zephyr.h>

static zephyr_interop_t *zephyr_interop;

bool ll_sys_time_init(void *os_interop) {
  zephyr_interop = (zephyr_interop_t*)os_interop;
  return (bool)zephyr_interop; 
}

ll_sys_time_t ll_sys_time_get_current_ticks(void) {

  ll_sys_time_t time;
  time.low_word = 0;
  time.high_word = 0;

  return time;
  
}


uint32_t ll_sys_time_get_clock_freq(void) {
  return 0;
}


bool ll_sys_time_set_wake_up(ll_sys_time_t absolute) {
  return false;
}

void ll_sys_sleep_ms(uint32_t ms) {
  
}
