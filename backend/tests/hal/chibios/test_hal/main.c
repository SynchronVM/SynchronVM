/*
    Copyright 2021 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

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

void print_it(const char *str, ...) {
  va_list args;

  chprintf((BaseSequentialStream *)&SDU1, str, args);
}


int main(void) {
  halInit();
  chSysInit();
  
  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   * Note, a delay is inserted in order to not have to disconnect the cable
   * after a reset.
   */
  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  usbConnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(500);


  chThdSleepMilliseconds(2000);


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

  chibios_register_dbg_print(print_it);
  
  while(true) {

    chprintf((BaseSequentialStream *)&SDU1, "Hello world\r\n");

    chThdSleepMilliseconds(500);
    
  }

  return 0; //unreachable
}
