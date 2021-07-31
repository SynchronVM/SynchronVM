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

#include <led.h>

#include <drivers/gpio.h>

#define LED_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define LED_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define LED_FLAGS(X)        DT_GPIO_FLAGS(DT_ALIAS(X), gpios)

#define CONFIG_LED_CASE(X) \
  case X: \
  led_drivers[(X)].pin = LED_PIN(svm_led##X);	\
  led_drivers[(X)].id  = (X); \
  led_drivers[(X)].state = false; \
  gpio_pin_configure(led_device, led_drivers[(X)].pin, GPIO_OUTPUT_ACTIVE | LED_FLAGS(svm_led##X)); \
  gpio_pin_set(led_device, led_drivers[(X)].pin, 0); \
  break;

led_driver_t led_drivers[10];
const struct device *led_device;

uint32_t led_num(void) {
  uint32_t num_leds = 0;

#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led0), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led1), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led2), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led3), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led4), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led5), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led6), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led7), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led8), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led9), okay)
   num_leds = num_leds + 1;
#endif
  return num_leds;
}

uint32_t led_identifiers(void) {
  uint32_t id_mask = 0;
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led0), okay)
  id_mask = id_mask | (1 << 0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led1), okay)
  id_mask = id_mask | (1 << 1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led2), okay)
  id_mask = id_mask | (1 << 2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led3), okay)
  id_mask = id_mask | (1 << 3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led4), okay)
  id_mask = id_mask | (1 << 4);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led5), okay)
  id_mask = id_mask | (1 << 5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led6), okay)
  id_mask = id_mask | (1 << 6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led7), okay)
  id_mask = id_mask | (1 << 7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led8), okay)
  id_mask = id_mask | (1 << 8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led9), okay)
  id_mask = id_mask | (1 << 9);
#endif
   return id_mask;
}

led_driver_t* led_init(uint32_t identifier) {
  
  if (!led_device) {
    /* assumption all leds will have the same device label */
    /* assumption there will be a led0 */
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led0), okay)
    led_device = device_get_binding(LED_DEVICE_LABEL(svm_led0));
#endif
  }

  if (!led_device) return false;

  switch(identifier) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led0), okay)
  CONFIG_LED_CASE(0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led1), okay)
  CONFIG_LED_CASE(1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led2), okay)
  CONFIG_LED_CASE(2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led3), okay)
  CONFIG_LED_CASE(3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led4), okay)
  CONFIG_LED_CASE(4);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led5), okay)
  CONFIG_LED_CASE(5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led6), okay)
  CONFIG_LED_CASE(6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led7), okay)
  CONFIG_LED_CASE(7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led8), okay)
  CONFIG_LED_CASE(8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_led9), okay)
  CONFIG_LED_CASE(9);
#endif
  default:
    return NULL;
  }
  return &led_drivers[identifier];
}

void led_set(led_driver_t *led, bool value) {

  if (led) {
    led->state = value;
    gpio_pin_set(led_device, led->pin, led->state);
  }
}
/* Look into this. Query the LED directly instead? */ 
bool led_state(led_driver_t *led) {
  bool s = false;
  if (led) {
    s = led->state;
  }
  return s;
}


/* LL interface implementation */

#include <ll/ll_led.h>
#include <string.h>

static uint32_t ll_led_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

static uint32_t ll_led_data_available(struct ll_driver_s *this) {
  return 1;
}

static uint32_t ll_led_data_writeable(struct ll_driver_s *this) {
  return 1;
}

/* sets bit 0 in the first byte of data to the value of the led.
    The rest of data is cleared. */ 
static uint32_t ll_led_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  led_driver_t *led_driver = (led_driver_t*)this->driver_info; 
  bool state = led_state(led_driver);

  if (data_size > 0) {
    memset(data,0, data_size);
    data[0] = state?1:0;
  }
  return data_size;
}

/* data[0] will be reinterpreted as a bool */
static uint32_t ll_led_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  led_driver_t *led_driver = (led_driver_t*)this->driver_info;
  if (data_size > 0) {
    led_set(led_driver, data[0]); 
  }
  return data_size; /* there are alternative interpretations one could make... */
}

bool ll_led_init(ll_driver_t* lld, uint32_t led_id, bool initial_state) {
  led_driver_t *led_driver = led_init(led_id);
  bool r = false;
  
  if (led_driver) {
    r = true; 
    led_set(led_driver, initial_state);
    lld->driver_info = (void*) led_driver;
    lld->is_synchronous = true;
    lld->ll_control_fun = ll_led_control;
    lld->ll_read_fun = ll_led_read;
    lld->ll_write_fun = ll_led_write;
    lld->ll_data_readable_fun = ll_led_data_available;
    lld->ll_data_writeable_fun = ll_led_data_writeable;
  }
  return r;
}

