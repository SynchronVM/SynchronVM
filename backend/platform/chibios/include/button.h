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

#ifndef BUTTON_H_
#define BUTTON_H_

#include <gpio.h>
#include <svm_chibios.h>
#include <svm_chibios_conf.h>

typedef struct {
  stm32_gpio_t *port;
  uint16_t pad;
  uint32_t id;
  uint32_t state;
  chibios_interop_t *interop;
} button_driver_internal_t;

// TODO: Check if it it can be added annonymously. 
#define BUTTON_DRIVER_INTERNAL button_driver_internal_t internal

extern void button_cb(void *arg);

#define BUTTON_DRIVER_INTERNAL_INTERRUPT_MODE(XbdrvX, XbidX, XMX )

//  #WARNING BUTTON Interrupt mode not configurable yet using the ChibiOs implementation


#define BUTTON_DRIVER_INTERNAL_INIT(XbdrvX, XbidX, Xdrv_idX, XcustomX) \
  {\
  palSetPadMode(BUTTON##XbidX##_GPIO,\
                BUTTON##XbidX##_PIN,\
                BUTTON##XbidX##_MODE);\
  \
  XbdrvX.internal.port = BUTTON##XbidX##_GPIO;\
  XbdrvX.internal.pad = BUTTON##XbidX##_PIN;\
  XbdrvX.internal.id = XbidX;\
  XbdrvX.internal.state = false;\
  XbdrvX.internal.interop = (chibios_interop_t*)XcustomX;\
  palEnablePadEvent(BUTTON##XbidX##_GPIO, BUTTON##XbidX##_PIN, BUTTON##XbidX##_EVENT_MODE);\
  palSetPadCallback(BUTTON##XbidX##_GPIO, BUTTON##XbidX##_PIN, button_cb, &XbdrvX);\
  }

#endif
