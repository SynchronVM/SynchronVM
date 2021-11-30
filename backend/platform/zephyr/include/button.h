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

#include <stdint.h>
#include <stdbool.h>

/* SVM includes */ 
#include <svm_zephyr.h>

/* Zephyr includes */ 
#include <drivers/gpio.h>

typedef struct {
  struct gpio_callback cb_data;
  uint32_t pin;
  uint32_t state;
  uint32_t drv_id;
  const struct device *dev;
  zephyr_interop_t *interop;
} button_driver_internal_t; 

#define BUTTON_DRIVER_INTERNAL button_driver_internal_t internal

extern void button_pressed_cb(const struct device *dev,
			      struct gpio_callback *cb,
			      uint32_t pin);

#define BUTTON_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define BUTTON_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define BUTTON_FLAGS(X)        (GPIO_INPUT | GPIO_INT_DEBOUNCE | DT_GPIO_FLAGS(DT_ALIAS(X), gpios))

#if DT_NODE_EXISTS(button_mode)
#define BUTTON_MODE(X) 
#else
#define BUTTON_MODE(X) "DT_INT_EDGE_BOTH"
#endif

#define BUTTON_DRIVER_INTERNAL_INTERRUPT_MODE(XbdrvX, XbidX, XMX) \
  if (XMX == GPIO_INTERRUPT_MODE_EDGE_TO_ACTIVE) {\
      gpio_pin_interrupt_configure(XbdrvX.internal.dev, XbdrvX.internal.pin, GPIO_INT_EDGE_TO_ACTIVE); \
  } else  if (XMX == GPIO_INTERRUPT_MODE_EDGE_TO_INACTIVE) {						\
    gpio_pin_interrupt_configure(XbdrvX.internal.dev, XbdrvX.internal.pin, GPIO_INT_EDGE_TO_INACTIVE); \
  } else { \
    gpio_pin_interrupt_configure(XbdrvX.internal.dev, XbdrvX.internal.pin, GPIO_INT_EDGE_BOTH); \
  } 

#define BUTTON_DRIVER_INTERNAL_INIT(XbdrvX, XbidX, Xdrv_idX, XcustomX)	\
  {\
  XbdrvX.internal.dev = NULL;\
  XbdrvX.internal.pin = BUTTON_PIN(svm_button##XbidX);\
  XbdrvX.internal.state = 0;\
  XbdrvX.internal.drv_id = Xdrv_idX;\
  XbdrvX.internal.interop = (zephyr_interop_t *)(XcustomX);\
  XbdrvX.internal.dev = device_get_binding(BUTTON_DEVICE_LABEL(svm_button##XbidX));\
  gpio_pin_configure(XbdrvX.internal.dev, XbdrvX.internal.pin, BUTTON_FLAGS(svm_button##XbidX)); \
  gpio_init_callback(&XbdrvX.internal.cb_data, button_pressed_cb, BIT(BUTTON_PIN(svm_button##XbidX))); \
  gpio_add_callback(XbdrvX.internal.dev, &XbdrvX.internal.cb_data);\
  }

#endif
