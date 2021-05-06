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

#include <stdint.h>
#include <stdbool.h>

#include <button.h>
#include <drivers/gpio.h>
#include <hal/zephyr/svm_zephyr.h>

#define BUTTON_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define BUTTON_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define BUTTON_FLAGS(X)        (GPIO_INPUT | DT_GPIO_FLAGS(DT_ALIAS(X), gpios))


#define CONFIG_BUTTON_CASE(X) \
  case X: \
  button_drivers[(X)].pin = BUTTON_PIN(svm_button##X);\
  button_drivers[(X)].id  = drv_id; \
  button_data[(X)].interop = (zephyr_interop_t*)backend_custom;\
  button_data[(X)].drv_id = drv_id; \
  gpio_pin_configure(button_device, button_drivers[(X)].pin, BUTTON_FLAGS(svm_button##X)); \
  gpio_pin_interrupt_configure(button_device, button_drivers[(X)].pin, GPIO_INT_EDGE_TO_ACTIVE); \
  gpio_init_callback(&button_data[X].cb_data, button_pressed, BIT(BUTTON_PIN(svm_button##X))); \
  gpio_add_callback(button_device, &button_data[X].cb_data);\
  break;

typedef struct {
  struct gpio_callback cb_data;
  zephyr_interop_t *interop;
  uint32_t drv_id;
}  button_user_data_t;


static button_user_data_t button_data[10];
button_driver_t button_drivers[10];
const struct device *button_device;

/* The callback routine */
static void button_pressed(const struct device *dev,
		    struct gpio_callback *cb,
		    uint32_t pins) {

  /* This is so weird and backwards!!! CONTAINER_OF*/

  button_user_data_t *parent = CONTAINER_OF(cb, button_user_data_t, cb_data);

  zephyr_interop_t *interop = parent->interop;

  ll_driver_msg_t msg; // nonsense message
  msg.driver_id = 77;  // why not!?
  msg.timestamp = 128;
  msg.data = 1;  // button is pressed

  if (interop->send_message(interop, msg) == -ENOMSG) {
    /* Message was not send due to queue being full. 
       What do we do in this case?  */ 
  }
}

uint32_t button_num(void) {
  uint32_t num_buttons = 0;

#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button0), okay)
  num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button1), okay)
  num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button2), okay)
  num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button3), okay)
  num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button4), okay)
   num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button5), okay)
   num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button6), okay)
   num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button7), okay)
   num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button8), okay)
   num_buttons = num_buttons + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button9), okay)
   num_buttons = num_buttons + 1;
#endif
  return num_buttons;
}


button_driver_t *button_init(uint32_t drv_id, void *backend_custom, uint32_t identifier){

  if (!button_device) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button0), okay) 
    button_device = device_get_binding(BUTTON_DEVICE_LABEL(svm_button0));
#endif
  }

  if (!button_device) return false;

  switch(identifier) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button0), okay)
    CONFIG_BUTTON_CASE(0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button1), okay)
    CONFIG_BUTTON_CASE(1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button2), okay)
    CONFIG_BUTTON_CASE(2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button3), okay)
    CONFIG_BUTTON_CASE(3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button4), okay)
    CONFIG_BUTTON_CASE(4);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button5), okay)
    CONFIG_BUTTON_CASE(5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button6), okay)
    CONFIG_BUTTON_CASE(6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button7), okay)
    CONFIG_BUTTON_CASE(7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button8), okay)
    CONFIG_BUTTON_CASE(8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_button9), okay)
    CONFIG_BUTTON_CASE(9);
#endif
  default:
    return NULL;
  }
  return &button_drivers[identifier];
}
