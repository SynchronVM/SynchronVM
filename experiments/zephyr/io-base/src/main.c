#include <zephyr/types.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <zephyr.h>

#include <sys/ring_buffer.h>
#include <usb/usb_device.h>
#include <drivers/uart.h>
#include <drivers/gpio.h>

/* Our own library of functions */
#include "usb_cdc.h"

#define PRINT usb_printf
//#define PRINT printk

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

/************/

void main(void)
{
  int err;


  start_usb_cdc_thread();

  const struct device *d_led0;
  const struct device *d_led1;
  
  d_led0 = device_get_binding(LED_DEVICE_LABEL(led0));
  d_led1 = device_get_binding(LED_DEVICE_LABEL(led1));
  gpio_pin_configure(d_led0, LED_PIN(led0), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led0));
  gpio_pin_configure(d_led1, LED_PIN(led1), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led1));

  k_sleep(K_SECONDS(4)); /* Give me a chance to connect the usb */   

  
  int led1_state = 1; 
  while(1) {

    gpio_pin_set(d_led1, LED_PIN(led1), led1_state);
    
    k_sleep(K_SECONDS(1));

    led1_state = 1 - led1_state;    
  }
}
