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

/*
 * The led0 devicetree alias is optional. If present, we'll use it
 * to turn on the LED whenever the button is pressed.
 */

void button_pressed(const struct device *dev, struct gpio_callback *cb,
		    uint32_t pins)
{
  //printk("Button pressed at %" PRIu32 "\n", k_cycle_get_32());
}

void main(void)
{
  int ret = 0;

  printk("starting up\n");

  const struct device *led_dev = device_get_binding(DT_GPIO_LABEL(LED_NODE, gpios));
  const struct device *but_dev = device_get_binding(DT_GPIO_LABEL(BUTTON_NODE,gpios));

  uint32_t led_pin = DT_GPIO_PIN(LED_NODE, gpios);
  uint32_t but_pin = DT_GPIO_PIN(BUTTON_NODE, gpios);

  if (led_dev == NULL) {
    printk("led device is null\n");
    return;
  }

  if (but_dev == NULL) {
    printk("button device is null\n");
    return;
  }

  ret = gpio_pin_configure(but_dev, but_pin, GPIO_INPUT | DT_GPIO_FLAGS(BUTTON_NODE, gpios));
  if (ret != 0) {
    printk("Error configure button\n");
    return;
  }

  /* ret = gpio_pin_interrupt_configure_dt(&button, */
  /* 				      GPIO_INT_EDGE_TO_ACTIVE); */
  /* if (ret != 0) { */
  /* 	printk("Error %d: failed to configure interrupt on %s pin %d\n", */
  /* 		ret, button.port->name, button.pin); */
  /* 	return; */
  /* } */

  /* gpio_init_callback(&button_cb_data, button_pressed, BIT(button.pin)); */
  /* gpio_add_callback(button.port, &button_cb_data); */
  /* printk("Set up button at %s pin %d\n", button.port->name, button.pin); */


  ret = gpio_pin_configure(led_dev, led_pin,  GPIO_OUTPUT | DT_GPIO_FLAGS(LED_NODE, gpios));
  if (ret != 0) {
    printk("error configuring led\n");
  } else {
    printk("LED OK!\n");
  }

  printk("Press the button\n");
  while (1) {
    int val = gpio_pin_get(but_dev, but_pin);
    gpio_pin_set(led_dev,led_pin, val);

    //k_msleep(SLEEP_TIME_MS);
  }
}
