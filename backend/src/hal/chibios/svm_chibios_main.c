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
  
  while (true) {
    chThdSleepMilliseconds(1000);
  }
  return 0;
}
