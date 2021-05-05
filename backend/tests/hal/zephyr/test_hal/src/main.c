#include <zephyr/types.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <zephyr.h>
#include <sys/printk.h>

#include <sys/ring_buffer.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#include <drivers/i2c.h>
#include <drivers/sensor.h>
#include <drivers/counter.h>

#include <kernel.h>

/* SenseVM include */
#include <svm_zephyr.h> /* <hal/zephyr/svm_zephyr.h> */
#include <vm-conf.h>
//#include <VMC.h>
//#include <CAM.h>

/* Our own library of stuff! */
#include "ll_uart.h"
#include "ll_led.h"

//#include "powerman.h"
//#include "timerman.h"

//#define PRINT usb_printf
#define PRINT printk

/* *************** */
/*   UART buffers  */

uint8_t uart0_in_buffer[1024];
uint8_t uart0_out_buffer[1024];

/**********/
/* Button */

#if DT_NODE_HAS_STATUS(DT_ALIAS(button0),okay)
#define BUTTON0_GPIO_LABEL DT_GPIO_LABEL(DT_ALIAS(button0), gpios)
#define BUTTON0_GPIO_PIN   DT_GPIO_PIN(DT_ALIAS(button0), gpios)
#define BUTTON0_GPIO_FLAGS (GPIO_INPUT | DT_GPIO_FLAGS(DT_ALIAS(button0), gpios))
#else
#error "No button defined in the devicetree"
#endif

static struct gpio_callback button_cb_data;

void button_pressed(const struct device *dev,
		    struct gpio_callback *cb,
		    uint32_t pins) {
  PRINT("Button pressed at %" PRIu32 "\n", k_cycle_get_32());
}


/* ****************** */
/* Thread info dumper */

static int t_counter = 0;

void t_info_dump(const struct k_thread *cthread, void *user_data) {
  struct k_thread *thread = (struct k_thread *)cthread;
  const char *tname;

  tname = k_thread_name_get(thread);

  PRINT("%s%p %-10s",
	(thread == k_current_get()) ? "*" : " ",
	thread,
	tname ? tname : "NA");
  PRINT("\toptions: 0x%x, priority: %d timeout: %lld",
	thread->base.user_options,
	thread->base.prio,
	thread->base.timeout.dticks);
  PRINT("\tstate: %s\r\n", k_thread_state_str(thread));

  t_counter++;

}
const struct device *button;

void main(void) {

  PRINT("Pause 2 seconds\r\n");
  k_sleep(K_SECONDS(2));

  PRINT("Initializing SenseVM Runtime System\r\n");
  zephyr_sensevm_init();

  PRINT("Number of containers: %d\r\n", VMC_NUM_CONTAINERS);

  PRINT("Starting SenseVM Containers Threads\r\n");
  if (!zephyr_start_container_threads()) {
    PRINT("FAILED! Could not start container threads\r\n");
  }

  k_sleep(K_SECONDS(1));

  PRINT("Currently running threads:\r\n");
  k_thread_foreach(t_info_dump, NULL);


  int ret = 0;
  
  PRINT("Initializing button\r\n");
  button = device_get_binding(BUTTON0_GPIO_LABEL);
  if (button == NULL) {
    printk("Error: didn't find %s device\n", BUTTON0_GPIO_LABEL);
    return;
  }

  ret = gpio_pin_configure(button, BUTTON0_GPIO_PIN, BUTTON0_GPIO_FLAGS);
  if (ret != 0) {
    printk("Error %d: failed to configure %s pin %d\n",
	   ret, BUTTON0_GPIO_LABEL, BUTTON0_GPIO_PIN);
    return;
  }
  
  ret = gpio_pin_interrupt_configure(button,
				     BUTTON0_GPIO_PIN,
				     GPIO_INT_EDGE_TO_ACTIVE);
  if (ret != 0) {
    printk("Error %d: failed to configure interrupt on %s pin %d\n",
	   ret, BUTTON0_GPIO_LABEL, BUTTON0_GPIO_PIN);
    return;
  }
  
  gpio_init_callback(&button_cb_data, button_pressed, BIT(BUTTON0_GPIO_PIN));
  gpio_add_callback(button, &button_cb_data);
  printk("Set up button at %s pin %d\n", BUTTON0_GPIO_LABEL, BUTTON0_GPIO_PIN);
  

  
  /* All the stuff below should move into zephyr_sensevm_init   *
   * Potentially the main "thread" can just die here or go      *
   * into an infinite loop that monitors the progress of the    *
   * container threads.                                         */

  /* ******************* */
  /* Configure some LEDs */

  ll_driver_t led0;
  ll_driver_t led1;

  if (ll_led_init(&led0, 0, 0)) {
    PRINT("LL_LED: OK init led0\r\n");
  } else {
    PRINT("LL_LED: FAILED init led0\r\n");
  }

  if (ll_led_init(&led1, 1, 1)) {
    PRINT("LL_LED: OK init led1\r\n");
  } else {
    PRINT("LL_LED: FAILED init led1\r\n");
  }

  /* configure uart */

  ll_driver_t uart_drv;

  PRINT("STARTING UARTS\r\n");
  if (ll_uart_init(&uart_drv, UART_IF0, uart0_in_buffer, 1024, uart0_out_buffer, 1024)) {
    PRINT("LL_UART: OK!\r\n");
  } else {
    PRINT("LL_UART: Failed!\r\n");
  }

  const char *hello = "hello world\r\n";

  int i = 0; 
  while (1) {

    ll_write(&uart_drv, (uint8_t*)hello, strlen(hello));
    uint8_t led0_state;
    uint8_t led1_state;

    ll_read(&led0, &led0_state, 1);
    ll_read(&led1, &led1_state, 1);

    led0_state = 1 - led0_state;
    led1_state = 1 - led1_state;

    ll_write(&led0, &led0_state, 1);
    ll_write(&led1, &led1_state, 1);

    if (i % 5 == 0) {
      k_sleep(K_MSEC(100));
      PRINT("Currently running threads:\r\n");
      k_thread_foreach(t_info_dump, NULL);
    }
    
    k_sleep(K_SECONDS(1));
    i++;
  }

}
