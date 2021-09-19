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

#ifndef CHIBIOS_LED_H_
#define CHIBIOS_LED_H_

#include <stdbool.h>
#include <stdint.h>

#include <gpio.h>
#include <svm_chibios_conf.h>

typedef struct {
  stm32_gpio_t *port;
  uint16_t pad;
  uint32_t id;
  bool state;
} led_driver_internal_t;

#define LED_DRIVER_INTERNAL led_driver_internal_t internal

#define LED_DRIVER_INTERNAL_INIT(XldrvX,XlidX,Xdrv_idX) \
    palSetPadMode(LED##XlidX##_GPIO,\
		  LED##XlidX##_PIN,\
		  LED##XlidX##_MODE);\
    palClearPad(LED##XlidX##_GPIO,\
		LED##XlidX##_PIN);\
    \
    XldrvX.internal.port = LED##XlidX##_GPIO;\
    XldrvX.internal.pad = LED##XlidX##_PIN;  \
    XldrvX.internal.id = Xdrv_idX;\
    XldrvX.internal.state = false; 

#endif
