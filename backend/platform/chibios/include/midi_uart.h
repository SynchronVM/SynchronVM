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

#ifndef MIDI_UART_H_
#define MIDI_UART_H_

#include <gpio.h>
#include <svm_chibios.h>
#include <svm_chibios_conf.h>

typedef struct {
  SerialConfig serial_config;
  stm32_gpio_t *port;
  uint16_t tx_pad;
  uint16_t rx_pad;
  SerialDriver *serial_driver;
  uint32_t id;
  chibios_interop_t *interop;
} midi_uart_driver_internal_t;

extern SerialConfig midi_uart_serial_def_conf;


#define MIDI_UART_DRIVER_INTERNAL midi_uart_driver_internal_t internal


#define MIDI_UART_DRIVER_INTERNAL_INIT(XmdrvX, XmidX, Xdrv_idX, XcustomX) \
  {\
  XmdrvX.internal.serial_config = midi_uart_serial_def_conf; \
  XmdrvX.internal.serial_driver = &MIDI##XmidX##_DRIVER; \
  XmdrvX.internal.id = XmidX; \
  XmdrvX.internal.port = MIDI##XmidX##_GPIO; \
  XmdrvX.internal.tx_pad = MIDI##XmidX##_TX; \
  XmdrvX.internal.rx_pad = MIDI##XmidX##_RX; \
  XmdrvX.internal.interop = (chibios_interop_t*)XcustomX;\
  palSetPadMode(MIDI##XmidX##_GPIO, \
                MIDI##XmidX##_TX, \
                MIDI##XmidX##_MODE); \
  palSetPadMode(MIDI##XmidX##_GPIO, \
                MIDI##XmidX##_RX, \
                MIDI##XmidX##_MODE); \
  sdStart(&MIDI##XmidX##_DRIVER, &XmdrvX.internal.serial_config); \
  }
 

/* palSetPadMode(GPIOC, 10, PAL_MODE_ALTERNATE(7)); */
/* palSetPadMode(GPIOC, 11, PAL_MODE_ALTERNATE(7)); */
/* sdStart(&SD3, &serial_cfg); */


/* #define UART_PORT GPIOC */
/* #define UART_TX   10 */
/* #define UART_RX   11 */
/* #define SERIAL_DRIVER SD3 */

#endif
