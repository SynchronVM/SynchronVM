/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar             		  */
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include "ch.h"
#include "hal.h"
#include "usbcfg.h"
#include "chprintf.h"

#include <svm_chibios.h>
#include <sys/sys_time.h>
#include "VMC.h"

/**********************************/
/* Debug printing function        */

char buf[2048];

void print_it(const char *str, va_list args) {

  memset(buf,0,2048);

  vsnprintf(buf, 2048, str, args);

  chprintf((BaseSequentialStream *)&SDU1,"%s", buf);
}

int main(void) {
  halInit();
  chSysInit();

  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  usbConnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(500);

  chThdSleepMilliseconds(2000);

  chprintf((BaseSequentialStream *)&SDU1,"Starting up!\r\n");

  chThdSleepMilliseconds(500);

  chibios_register_dbg_print(print_it);

  if (!chibios_sensevm_init()) {
     chprintf((BaseSequentialStream *)&SDU1, "SenseVM init failed!\r\n");
  } else {
    chprintf((BaseSequentialStream *)&SDU1, "SenseVM initialized!\r\n");
  }

  if (!chibios_start_container_threads()) {
     chprintf((BaseSequentialStream *)&SDU1, "SenseVM failed to start container threads!\r\n");
  } else {
    chprintf((BaseSequentialStream *)&SDU1, "SenseVM container threads started!\r\n");
  }

  // Why is this needed ??
  // Why can we not just let the main thread die here ?

  systime_t last_time = 0;

  static const char *states[] = {CH_STATE_NAMES};

  char buf[64];

  uint32_t tot_gc_time_last = 0;
  
  while (true) {
    chThdSleepMilliseconds(4000);
    thread_t *tp;
    
    tp = chRegFirstThread();
    chprintf((BaseSequentialStream *)&SDU1,"-----------------------------------------------------\r\n");
    do {
      double cpu_usage = 100.0 * ((float)tp->time / (float)(chVTGetSystemTimeX() - last_time));

      const char *tstate = states[tp->state];
      snprintf(buf,64,"%.1f", cpu_usage);
      
      chprintf((BaseSequentialStream *)&SDU1,"%14s %14s %8s %u\r\n", tp->name, tstate, buf, tp->time); 
      tp->time = 0;
      tp = chRegNextThread(tp);
    } while (tp != NULL);
    last_time = chVTGetSystemTimeX();

    vmc_statistics_t stats;
    vmc_get_stats(&stats);
    float avg = (float)stats.gc_time_total / (float)stats.gc_num;
    snprintf(buf, 64, "%f", avg);
    chprintf((BaseSequentialStream *)&SDU1,"GC time avg: %s us\r\n", buf);
    snprintf(buf, 64, "%f", 0.0001 *  (float)(stats.gc_time_total - tot_gc_time_last));
    chprintf((BaseSequentialStream *)&SDU1,"GC time tot (last 4 seconds): %s ms\r\n", buf);
    chprintf((BaseSequentialStream *)&SDU1,"GC time min: %d us\r\n", stats.gc_time_min);
    chprintf((BaseSequentialStream *)&SDU1,"GC time max: %d us\r\n", stats.gc_time_max);
    tot_gc_time_last = stats.gc_time_total;
            
  }

 
  
  return 0;
}
