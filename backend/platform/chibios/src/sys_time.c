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

#include <ch.h>
#include <hal.h>

#include <chprintf.h> // DEBUG
#include <usbcfg.h>   // DEBUG

#include <sys/sys_time.h>
#include <svm_chibios.h>

#include <svm_chibios_conf.h>

/* This file is stm32 specific */
#include "stm32_tim.h"
#include "stm32_rcc.h"

#define COMB_EXPAND0(X,Y) X##Y
#define COMB_EXPAND(X,Y) COMB_EXPAND0(X,Y)

#define TIM COMB_EXPAND(STM32_TIM, SYS_TIMER);

stm32_tim_t * tim = TIM;

chibios_interop_t *interop;

typedef struct {
  bool active;
  Time alarm_time;
} sys_time_alarm_t;

static sys_time_alarm_t alarm;
static volatile uint32_t counter_high_word;
static uint32_t counter_freq;


bool sys_time_init(void *os_interop) {

  if (!os_interop) return false;

  interop = (chibios_interop_t*)os_interop;

  counter_high_word = 0xFFFFFFFF;
  counter_freq = 0; /* TODO: Figure out how to compute this.
  		       I think it is 84Mhz / (tim->PSC+1)
  		     */

  alarm.active = false;
  alarm.alarm_time = 0;


  COMB_EXPAND(rccEnableTIM, SYS_TIMER)(true);
  COMB_EXPAND(rccResetTIM, SYS_TIMER)();

  nvicEnableVector(COMB_EXPAND(STM32_TIM,COMB_EXPAND(SYS_TIMER,_NUMBER)) ,
  		   COMB_EXPAND(STM32_GPT_TIM, COMB_EXPAND(SYS_TIMER, _IRQ_PRIORITY))); /* use GPT level prio */


  tim->PSC = 0; //0xFFFF;     // counter rate is input_clock / (0xFFFF+1)
  tim->ARR = 0xFFFFFFFF; // Value when counter should flip to zero.

  tim->CCR[0] = 0xFFFFFFFF; /* init compare values */
  tim->CCR[1] = 0x0;
  tim->CCR[2] = 0x0;
  tim->CCR[3] = 0x0;

  tim->CCER = 0x1; /* activate compare on ccr channel 1 */

  /* TODO: make sure we get "greater than or equal to" comparison on the CCR */

  tim->CNT = 0;
  tim->EGR = 0x1; // Update event (Makes all the configurations stick)

  tim->CR1 = 0x1; // enable
  tim->DIER |= 0x1; /* activate interrupt on "update event" (for example overflow) */

  return true;
}

OSAL_IRQ_HANDLER(COMB_EXPAND(STM32_TIM,COMB_EXPAND(SYS_TIMER, _HANDLER))) {
  OSAL_IRQ_PROLOGUE();
  
  uint32_t hw = counter_high_word;
  
  if ((tim->SR & tim->DIER) & 0x1 ) { /* This indicates and update event (overflow?) */
    /* TODO: Not 100% certain this is definitely an overflow. Couldn't it
       be other "events"?  */
    uint32_t sr = tim->SR;
    sr &= 0x1 & STM32_TIM_DIER_IRQ_MASK;
    tim->SR = ~sr; /* clear update event flag */


    counter_high_word++;

    if (alarm.active) {
      if (counter_high_word == alarm.alarm_time >> 32) {
     	tim->CCR[0] = (uint32_t)alarm.alarm_time;
    	tim->DIER |= 0x2; /* activate interrupt on ccr channel 1 */
      }
    }
  }

  if ((tim->SR & tim->DIER) & 0x2) {
    uint32_t low_word;
    //uint32_t high_word;
    Time time;
    
    uint32_t sr = tim->SR;
    sr &= 0x2 & STM32_TIM_DIER_IRQ_MASK;
    tim->SR = ~sr; /* clear ccr event flag */


    tim->DIER &= ~0x2; /* disable interrupt on ccr channel 1 */

    low_word = tim->CNT;
    time = hw;
    time <<= 32;
    time |= low_word;
    
    svm_msg_t msg;
    msg.sender_id = SYS_TIME_SENDER_ID;
    msg.timestamp = time; //sys_time_get_current_ticks();
    msg.data = 0xDEADBEEF;
    msg.msg_type = 0;

    osalSysLockFromISR(); 
    interop->send_message(interop, msg); /* check for error */
    osalSysUnlockFromISR();  

    alarm.active = false;
  }
  
  OSAL_IRQ_EPILOGUE();
}


Time sys_time_get_current_ticks(void) {
  chSysLock();
  Time time = 0;
  uint32_t low_word;
  uint32_t high_word;
  //uint32_t high_word2;

  /* do { */
  /*   high_word = counter_high_word; */
  /*   low_word = tim->CNT; */
  /*   high_word2 = counter_high_word; */
  /* } while (high_word != high_word2); /\* todo execute body at most twice *\/ */

  high_word = counter_high_word;
  low_word = tim->CNT;
  
  time = high_word;
  time <<= 32;
  time |= low_word;
  chSysUnlock();
  return time;
}

uint32_t sys_time_alarm_channels(void) {
  return 4;
}

uint32_t sys_time_get_clock_freq(void) {

  return counter_freq;
}

bool sys_time_set_wake_up(Time absolute) {

  chSysLock();
  uint32_t high_word = 0;

  alarm.alarm_time = absolute;
  alarm.active = true;

  high_word = counter_high_word;
  if (high_word == alarm.alarm_time >> 32) {
    if ((uint32_t)absolute < tim->CNT) {
      tim->CCR[0] = tim->CNT + 1000;
    } else {
      tim->CCR[0] = (uint32_t)absolute; /* low 32 bits */
    }
    tim->DIER |= 0x2; /* enable interrups on CCR[0] */
  } else {
    tim->DIER &= ~0x2;
  }

  /* uint32_t high_word = 0; */
  /* uint32_t high_word2 = 0; */

  /* alarm.alarm_time = absolute; */
  /* alarm.active = true; */

  /* do { */
  /*   high_word = counter_high_word; */
  /*   if (high_word == alarm.alarm_time >> 32) { */
  /*     tim->CCR[0] = absolute; /\* low 32 bits *\/ */
  /*     tim->DIER |= 0x2; /\* enable interrups on CCR[0] *\/ */
  /*     /\* if we manage to set this alarm, but high_word != high_word2 */
  /*        then the event should go off immediately *\/ */
  /*   } */
  /*   high_word2 = counter_high_word; */
  /* } while (high_word != high_word2); */

  //uint32_t low_word_now = tim->CNT;
  
  chSysUnlock();
  //chprintf((BaseSequentialStream *)&SDU1, "alarm set at high word: %u\r\n", high_word);
  //chprintf((BaseSequentialStream *)&SDU1, "Now low word         : %u\r\n", low_word_now);
  //chprintf((BaseSequentialStream *)&SDU1, "alarm set at low word: %u\r\n", tim->CCR[0]);
  return true;
}

uint32_t sys_get_timestamp(void) {
  systime_t t = chVTGetSystemTime();
  return (uint32_t) (100 * t);
}

Time sys_get_wake_up_time(void){
  return alarm.alarm_time;
}

bool sys_is_alarm_set(void){
  
  return alarm.active;
}


void sys_sleep_ms(uint32_t ms) {
  chThdSleepMilliseconds(ms);
}
