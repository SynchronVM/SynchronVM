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

#include "led.h"

#include <drivers/gpio.h>

#define LED_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define LED_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define LED_FLAGS(X)        DT_GPIO_FLAGS(DT_ALIAS(X), gpios)

#define CONFIG_LED_CASE(X) \
  case X: \
  led_drivers[(X)].pin = LED_PIN(led##X);	\
  led_drivers[(X)].id  = (X); \
  led_drivers[(X)].state = false; \
  gpio_pin_configure(led_device, led_drivers[(X)].pin, GPIO_OUTPUT_ACTIVE | LED_FLAGS(ssm_led##X)); \
  gpio_pin_set(led_device, led_drivers[(X)].pin, 0); \
  break;

led_driver_t led_drivers[10];
const struct device *led_device;

uint32_t led_num(void) {
  uint32_t num_leds = 0;

#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led0), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led1), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led2), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led3), okay)
  num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led4), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led5), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led6), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led7), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led8), okay)
   num_leds = num_leds + 1;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led9), okay)
   num_leds = num_leds + 1;
#endif
  return num_leds;
}

uint32_t led_identifiers(void) {
  uint32_t id_mask = 0;
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led0), okay)
  id_mask = id_mask | (1 << 0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led1), okay)
  id_mask = id_mask | (1 << 1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led2), okay)
  id_mask = id_mask | (1 << 2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led3), okay)
  id_mask = id_mask | (1 << 3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led4), okay)
  id_mask = id_mask | (1 << 4);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led5), okay)
  id_mask = id_mask | (1 << 5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led6), okay)
  id_mask = id_mask | (1 << 6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led7), okay)
  id_mask = id_mask | (1 << 7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led8), okay)
  id_mask = id_mask | (1 << 8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led9), okay)
  id_mask = id_mask | (1 << 9);
#endif
   return id_mask;
}

led_driver_t* led_init(uint32_t identifier) {

  if (!led_device) {
    /* assumption all leds will have the same device label */
    led_device = device_get_binding(LED_DEVICE_LABEL(led0));
  }

  if (!led_device) return false;

  switch(identifier) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led0), okay)
  CONFIG_LED_CASE(0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led1), okay)
  CONFIG_LED_CASE(1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led2), okay)
  CONFIG_LED_CASE(2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led3), okay)
  CONFIG_LED_CASE(3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led4), okay)
  CONFIG_LED_CASE(4);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led5), okay)
  CONFIG_LED_CASE(5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led6), okay)
  CONFIG_LED_CASE(6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led7), okay)
  CONFIG_LED_CASE(7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led8), okay)
  CONFIG_LED_CASE(8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(ssm_led9), okay)
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

