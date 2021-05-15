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

/* Our own library of stuff! */
#include "ll_uart.h"
#include "ll_led.h"

#include "powerman.h"
#include "timerman.h"

//#define PRINT usb_printf
#define PRINT printk

/* *************** */
/*   UART buffers  */

uint8_t uart0_in_buffer[1024];
uint8_t uart0_out_buffer[1024];


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

void main(void) {

  PRINT("Pause 5 seconds\r\n");
  //k_sleep(K_SECONDS(5));

  /* ***************** */
  /* Register powerman */ 
  //powerman_init();

  PRINT("POWERMAN: started\r\n");
  
  //k_sleep(K_SECONDS(5));

  k_thread_foreach(t_info_dump, NULL);

  PRINT("t_info_dump called %d times\r\n", t_counter);
  
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
  if (ll_uart_init(&uart_drv, UART_IF1, uart0_in_buffer, 1024, uart0_out_buffer, 1024)) {
    PRINT("LL_UART: OK!\r\n");
  } else {
    PRINT("LL_UART: Failed!\r\n");
  }
  PRINT("UARTS STARTED\r\n");
  
  const char *hello = "hello world\r\n"; 
 
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
    
    k_sleep(K_SECONDS(1));
  }

}
