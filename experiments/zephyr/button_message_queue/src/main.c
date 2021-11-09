/*
 * Copyright (c) 2016 Open-RnD Sp. z o.o.
 * Copyright (c) 2020 Nordic Semiconductor ASA
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <zephyr.h>
#include <device.h>
#include <drivers/gpio.h>
#include <sys/util.h>
#include <sys/printk.h>
#include <inttypes.h>

#define SLEEP_TIME_MS	1



#define BUTTON_NODE	DT_ALIAS(svm_button0)
#define LED_NODE        DT_ALIAS(svm_led0)

static struct gpio_callback button_cb_data;

const struct device *but_dev;
uint32_t but_pin;

// Construct a message queue
K_MSGQ_DEFINE(message_queue, sizeof(int), 100, 4);


void button_pressed(const struct device *dev, struct gpio_callback *cb,
		    uint32_t pins)
{
  int s = gpio_pin_get(but_dev, but_pin);

  k_msgq_put(&message_queue,&s, K_NO_WAIT);
}

void main(void)
{

  int ret = 0;

  printk("starting up\n");
  
  const struct device *led_dev = device_get_binding(DT_GPIO_LABEL(LED_NODE, gpios));
  but_dev = device_get_binding(DT_GPIO_LABEL(BUTTON_NODE,gpios));
  
  	
  uint32_t led_pin = DT_GPIO_PIN(LED_NODE, gpios);
  but_pin = DT_GPIO_PIN(BUTTON_NODE, gpios);

  if (led_dev == NULL) {
    printk("led device is null\n");
    return;
  }
  
  if (but_dev == NULL) {
    printk("button device is null\n");
    return;
  }
	
  ret = gpio_pin_configure(but_dev, but_pin, GPIO_INPUT);
  if (ret != 0) {
    printk("Error configure button\n");
    return;
  }

  ret = gpio_pin_interrupt_configure(but_dev, but_pin,
					GPIO_INT_EDGE_BOTH);
  if (ret != 0) {
    printk("Error configure interrupt\n");
    return;
  }

  gpio_init_callback(&button_cb_data, button_pressed, BIT(but_pin));
  gpio_add_callback(but_dev, &button_cb_data);

  ret = gpio_pin_configure(led_dev, led_pin,  GPIO_OUTPUT);
  if (ret != 0) {	  
    printk("error configuring led\n");
  } else {
    printk("LED OK!\n");
  }

 
  printk("Press the button\n");
  while (1) {

    int s = 1;
    
    k_msgq_get(&message_queue, (void*)&s, K_FOREVER);
    gpio_pin_set(led_dev, led_pin, s);
    
  }
}
