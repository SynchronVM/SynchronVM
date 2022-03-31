
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include "ch.h"
#include "hal.h"
#include "usbcfg.h"
#include "chprintf.h"

BaseSequentialStream *chp = NULL;

#define MAX_MESSAGES 2048

static mailbox_t mb;
static msg_t b[MAX_MESSAGES];

static int send_message(uint32_t msg) {

  int r = 1;

  msg_t msg_val = chMBPostI(&mb, msg);

  if (msg_val == MSG_OK) {
    r = 0;
  } 
  return r;
}

static int block_message(uint32_t *m) {

  msg_t msg_value;
  
  int r = chMBFetchTimeout(&mb, &msg_value, TIME_INFINITE);

  if (r == MSG_OK) {
    *m = msg_value;
    r = 1;
  } else {
    r = 0;
  } 
  return r;
}

void button_cb(void *arg) {
  (void) arg;

  chSysLockFromISR();
 
  uint32_t state = palReadPad(GPIOB, 11);

  if (send_message(state) == -1) {
    /* Message was not send due to queue being full. 
       What do we do in this case?  */ 
  }

  chSysUnlockFromISR();
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
  chp = (BaseSequentialStream*)&SDU1;

  chMBObjectInit(&mb, b, MAX_MESSAGES);

   palSetPadMode(GPIOB,
                11,
                PAL_MODE_INPUT);
   palEnablePadEvent(GPIOB, 11, PAL_EVENT_MODE_BOTH_EDGES); 
   palSetPadCallback(GPIOB, 11, button_cb, NULL); 

   
   palSetPadMode(GPIOB, 
		 12,
		 PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
   palClearPad(GPIOB,
	       12);
  
  
  while(true) {

    uint32_t m;
    int r = block_message(&m);
    if (r == 1) { 
      palWritePad(GPIOB, 12, m);
    } else {
      palClearPad(GPIOB, 12);
    }

    
  }

  return 0; //unreachable
}
