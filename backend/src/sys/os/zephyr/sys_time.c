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
#include <zephyr/types.h>
#include <drivers/counter.h>

#include <sys/sys_time.h>
#include <hal/zephyr/svm_zephyr.h>

#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_sys_timer), okay)
#else
#error "No system timer"
#endif

#define COUNTER DT_LABEL(DT_ALIAS(svm_sys_timer))

struct counter_alarm_cfg alarm_cfg;
struct counter_top_cfg overflow_cfg; 

static const struct device *counter_dev = NULL;

static zephyr_interop_t *zephyr_interop;

static uint32_t      counter_high_word;
static uint32_t      counter_freq;


void alarm_callback(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {
  /* send a message via the interop */
}

void overflow_callback(const struct device *dev, void* user_data) {
  counter_high_word ++;
}


/**************************************/
/* Interface functions implementation */

bool sys_time_init(void *os_interop) {


  counter_high_word = 0;
  
  zephyr_interop = (zephyr_interop_t*)os_interop;
  if (!zephyr_interop) return false;
 
  counter_dev = device_get_binding(COUNTER);
  if (!counter_dev) return false; 

  /* counter_freq = counter_get_frequency(counter_dev); */

  /* overflow_cfg.ticks = UINT_MAX; */
  /* overflow_cfg.callback = overflow_callback; */
  /* overflow_cfg.flags = 0; */
  /* overflow_cfg.user_data = NULL; */
  
  /* if (!counter_set_top_value(counter_dev, &overflow_cfg)) return false; */

  /* /\* just prepare the alarm config. Dont actually set this alarm. *\/ */
  /* alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE; */
  /* alarm_cfg.ticks = 0; */
  /* alarm_cfg.callback = alarm_callback; */
  /* alarm_cfg.user_data = &alarm_cfg; */

  /* /\* an alarm in the past should go off immediately *\/ */
  /* if (!counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET)) */
  /*   return false; */

  /* if (counter_start(counter_dev) != 0) return false; */

  return true;
}

Time sys_time_get_current_ticks(void) {

  Time time = 0;
  uint32_t low_word;
  uint32_t high_word;

  /* May need more sophisticated approach here 
     to really rule out overflow interleaving with 
     reading of low and high word */
  //uint32_t key = irq_lock();
  if (counter_dev) { 
    counter_get_value(counter_dev, &low_word);
  } else {
    low_word = 0;
  }
  high_word = counter_high_word;
  //irq_unlock(key);

  time = high_word;
  time <<= 32;
  time |= low_word;
  
  return time;
}

uint32_t sys_time_get_clock_freq(void) {
  return counter_freq;
}

// TODO: IMPLEMENT
bool sys_time_set_wake_up(Time absolute) {
  return false;
}

void sys_sleep_ms(uint32_t ms) {
  
}
