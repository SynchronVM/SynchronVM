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

#include <stdbool.h>
#include <string.h>
#include <led.h>

#include <drivers/gpio.h>

#if DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
#else
#error "NO LED0"
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led1), okay)
#else
#error "NO LED1"
#endif

#define LED_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define LED_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define LED_FLAGS(X)        DT_GPIO_FLAGS(DT_ALIAS(X), gpios)

#define CONFIG_LED(X) \
  led_devices[(X)] = device_get_binding(LED_DEVICE_LABEL(led##X));	\
  if (led_devices[(X)]) {\
    gpio_pin_configure(led_devices[(X)], LED_PIN(led##X), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led##X));\
    gpio_pin_set(led_devices[(X)], LED_PIN(led##X), 0);		\
    num_leds++;\
  }

#define LED_SET_CASE(X,V)			\
  case X:\
  led_states[(X)] = (V);						\
  gpio_pin_set(led_devices[(X)], LED_PIN(led##X), led_states[(X)]);	\
  break;\
  


const struct device *led_devices[10];
static int led_states[10];

static uint32_t num_leds; 

bool led_init(void) {

  num_leds = 0;
  /* all led_devices NULL 
     and all states 0 */ 
  for (int i = 0; i < 10; i++) { 
    led_devices[i] = NULL;
    led_states[i] = 0;
  }

#if DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
  CONFIG_LED(0);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led1), okay)
  CONFIG_LED(1);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led2), okay)
  CONFIG_LED(2);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led3), okay)
  CONFIG_LED(3);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led4), okay)
  CONFIG_LED(4); 
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led5), okay)
  CONFIG_LED(5);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led6), okay)
  CONFIG_LED(6);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led7), okay)
  CONFIG_LED(7);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led8), okay)
  CONFIG_LED(8);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led9), okay)
  CONFIG_LED(9);
#endif

  if (num_leds == 0) return false;

  return true;
}

void set_led(int led, int value) {
  switch(led) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
    LED_SET_CASE(0,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led1), okay)
    LED_SET_CASE(1,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led2), okay)
    LED_SET_CASE(2,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led3), okay)
    LED_SET_CASE(3,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led4), okay)
    LED_SET_CASE(4,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led5), okay)
    LED_SET_CASE(5,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led6), okay)
    LED_SET_CASE(6,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led7), okay)
    LED_SET_CASE(7,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led8), okay)
    LED_SET_CASE(8,value);
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led9), okay)
    LED_SET_CASE(9,value);
#endif    
  default:
    break;
  }
}
