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

typedef struct {
  struct counter_alarm_cfg alarm_cfg;
  bool  active;
  Time  alarm_time;
}sys_time_alarm_t;


static sys_time_alarm_t alarm;

static struct counter_top_cfg overflow_cfg;

static const struct device *counter_dev = NULL;

static zephyr_interop_t *zephyr_interop;

static volatile uint32_t      counter_high_word;
static uint32_t      counter_freq;


void alarm_callback(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {
  /* send a message via the interop */
  unsigned int key = irq_lock(); /* Do we need this? */

  svm_msg_t msg;
  msg.sender_id = SYS_TIME_SENDER_ID;
  msg.timestamp = sys_time_get_current_ticks();
  msg.data = 0xDEADBEEF;
  msg.msg_type = 0;

  if(zephyr_interop->send_message(zephyr_interop, msg) != -ENOMSG) {
    /* message error. what to do ? */
  }

  alarm.active = false;
  irq_unlock(key);
}

void overflow_callback(const struct device *dev, void* user_data) {
  counter_high_word ++;

  if (alarm.active) {

    if (counter_high_word == alarm.alarm_time >> 32) {

      alarm.alarm_cfg.ticks = alarm.alarm_time; /* low 32 bits */
      counter_set_channel_alarm(counter_dev,0, &alarm.alarm_cfg);
    }
  }
}

/**************************************/
/* Interface functions implementation */

bool sys_time_init(void *os_interop) {

  counter_high_word = 0;

  zephyr_interop = (zephyr_interop_t*)os_interop;
  if (!zephyr_interop) return false;

  counter_dev = device_get_binding(COUNTER);
  if (counter_dev == NULL) return false;

  counter_freq = counter_get_frequency(counter_dev);

  overflow_cfg.ticks = UINT_MAX;
  overflow_cfg.callback = overflow_callback;
  overflow_cfg.flags = 0;
  overflow_cfg.user_data = NULL;

  if (counter_set_top_value(counter_dev, &overflow_cfg) != 0) return false;

  /* just prepare the alarm config. Dont actually set this alarm. */
  alarm.alarm_cfg.flags = COUNTER_ALARM_CFG_ABSOLUTE | COUNTER_ALARM_CFG_EXPIRE_WHEN_LATE;
  alarm.alarm_cfg.ticks = 0;
  alarm.alarm_cfg.callback = alarm_callback;
  alarm.alarm_cfg.user_data = NULL;

  /* an alarm in the past should go off immediately */
  if (counter_set_guard_period(counter_dev, UINT_MAX/2, COUNTER_GUARD_PERIOD_LATE_TO_SET) != 0)
    return false;

  if (counter_start(counter_dev) != 0) return false;

  alarm.active = false;
  alarm.alarm_time = 0;

  return true;
}

Time sys_time_get_current_ticks(void) {

  if (!counter_dev) return 0;

  Time time = 0;
  uint32_t low_word;
  uint32_t high_word;
  uint32_t high_word2;

  /* May need more sophisticated approach here
     to really rule out overflow interleaving with
     reading of low and high word */
  do {
  high_word = counter_high_word;
  counter_get_value(counter_dev, &low_word);
  high_word2 = counter_high_word;
  } while (high_word != high_word2);  /* TODO: Execute at most twice */

  time = high_word;
  time <<= 32;
  time |= low_word;

  return time;
}

uint32_t sys_time_get_alarm_channels(void) {
  return counter_get_num_of_channels(counter_dev);
}

uint32_t sys_time_get_clock_freq(void) {
  return counter_freq;
}

bool sys_time_set_wake_up(Time absolute) {

  uint32_t high_word = 0;
  uint32_t high_word2 = 0;

  /* If an alarm is set on the channel, cancel it. */
  counter_cancel_channel_alarm(counter_dev, 0);

  alarm.alarm_time = absolute;
  alarm.active = true;
  alarm.alarm_cfg.ticks = absolute; /* low 32 bits */

  /* TODO: testing. this is tricky */
  /* TODO: fail if this loop performs more than 2 iterations */
  do {
    high_word = counter_high_word;
    if (high_word == alarm.alarm_time >> 32) {
      counter_set_channel_alarm(counter_dev,0, &alarm.alarm_cfg);
    }
    high_word2 = counter_high_word;
  } while (high_word != high_word2);

  return true;
}

Time sys_get_wake_up_time(void){
  return alarm.alarm_time;
}

bool sys_is_alarm_set(void){
  return alarm.active;
}

void sys_sleep_ms(uint32_t ms) {
  k_sleep(K_MSEC(ms));
}
