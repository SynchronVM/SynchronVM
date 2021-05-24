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


#include <ll/ll_driver.h>

void (*ll_driver_sleep_ms_fun)(uint32_t ms);
uint64_t (*ll_driver_timestamp_fun)();

bool ll_driver_init(void (*sleep_ms)(uint32_t),
		    uint64_t (*timestamp)()) {
  ll_driver_sleep_ms_fun = sleep_ms;
  ll_driver_timestamp_fun = timestamp;
  return true; /* maybe something will need a status indicator in the future */
}

void ll_driver_sleep_ms(uint32_t ms) {
  if (ll_driver_sleep_ms_fun) {
    ll_driver_sleep_ms_fun(ms);
  }
}

uint64_t ll_driver_timestamp() {
  uint64_t ts = 0;
  if (ll_driver_timestamp_fun) {
    ts = ll_driver_timestamp_fun();
  }
  return ts;
}

