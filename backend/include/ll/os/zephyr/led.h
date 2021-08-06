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

#ifndef LED_H_
#define LED_H_

#include <stdbool.h>
#include <stdint.h>

typedef struct {
  uint32_t pin;
  uint32_t id;
  bool state;
  const struct device *dev;
} led_driver_internal_t;

#define LED_DRIVER_INTERNAL led_driver_internal_t internal

#define LED_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define LED_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define LED_FLAGS(X)        DT_GPIO_FLAGS(DT_ALIAS(X), gpios)


#define LED_DRIVER_INTERNAL_INIT(XldrvX,XlidX,Xdrv_idX)			\
  XldrvX.internal.pin = LED_PIN(svm_led##XlidX);			\
  XldrvX.internal.id  = (Xdrv_idX);					\
  XldrvX.internal.state = false;					\
  XldrvX.internal.dev = device_get_binding(LED_DEVICE_LABEL(svm_led##XlidX)); \
  gpio_pin_configure(XldrvX.internal.dev, XldrvX.internal.pin, GPIO_OUTPUT_ACTIVE | LED_FLAGS(svm_led##XlidX)); \
  gpio_pin_set(XldrvX.internal.dev, XldrvX.internal.pin, 0); \

#endif
