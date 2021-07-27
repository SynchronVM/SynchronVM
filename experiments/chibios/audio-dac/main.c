
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


/* Audio DAC */

/* Analog in or LRCK */
/* LRCK - Left Right Clock 
   Determines if data on serial line goes to left or right channel */
#define AUDIO_LRCK_GPIO GPIOA
#define AUDIO_LRCK_PIN  4

#define AUDIO_AIN1X_GPIO GPIOA
#define AUDIO_AIN1X_PIN  4

/* Serial Control Clock */
#define AUDIO_SCL_GPIO GPIOB
#define AUDIO_SCL_PIN  6

/* Serial Control Data */
#define AUDIO_SDA_GPIO GPIOB
#define AUDIO_SDA_PIN  9

/* Master clock input - clocks the delta-sigma modulators */
#define AUDIO_MCLK_GPIO GPIOC
#define AUDIO_MCLK_PIN  7

/* Serial clock (audio data interface clock) */
#define AUDIO_SCLK_GPIO GPIOC
#define AUDIO_SCLK_PIN  10

/* Serial data (audio data input - two's complement serial audio data) */
#define AUDIO_SDIN_GPIO GPIOC
#define AUDIO_SDIN_PIN  12

/* Reset pin. Resets when pin is LOW. */ 
#define AUDIO_RESET_GPIO GPIOD
#define AUDIO_RESET_PIN  4





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

  while(true) {
    chprintf(chp,"Hello world\r\n");
    chThdSleepMilliseconds(1000);
  }

  return 0; //unreachable
}
