#include <zephyr/types.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <zephyr.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <sys/ring_buffer.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#include <drivers/i2c.h>
#include <drivers/sensor.h>
#include <drivers/counter.h>

#include "fib.h"
#include "blinky.h"

/* Our own library of stuff! */
//#include "defines.h"
#include "usb_cdc.h"
//#include "ltr_303als.h"
//#include "bme280.h"
#include "uart.h"
#include "ll_uart.h"
//#include "powerman.h"
#include "led.h"

//#define PRINT usb_printf
#define PRINT printk

/* ********** */
/*   TIMER    */

#define TIMER DT_LABEL(DT_NODELABEL(timer4))
struct  counter_alarm_cfg alarm_cfg;

const struct device *counter_dev = NULL;

/* *************** */
/*   UART buffers  */

ll_driver_t uart_drv;
uart_dev_t uart0;

uint8_t uart0_in_buffer[1024];
uint8_t uart0_out_buffer[1024];

void top_return(act_t *act)
{
  return;
}

act_t top = { .step = top_return };

/* ************************************************************ */
/* hw_tick                                                      */
/* ************************************************************ */

struct k_mbox tick_mbox;

void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) {

  /* Put a tick on the messagebox */ 
  struct k_mbox_msg send_msg;
  
  send_msg.info = 101;
  send_msg.size = 0;
  send_msg.tx_data = NULL;
  send_msg.tx_target_thread = K_ANY;

  /* third argument in async put is a semaphore  
     that can be set to NULL if not "needed" _*/
  k_mbox_async_put(&tick_mbox, &send_msg, NULL);
}


K_THREAD_STACK_DEFINE(tick_thread_stack, 512);
struct k_thread tick_thread;

sv_int_t r;

void tick_thread_main(void * a, void* b, void *c) {
  (void)a;
  (void)b;
  (void)c;

  struct k_mbox_msg recv_msg;

  //sv_int_t r;
  initialize_int(&r);
  r.value = 0;
  now = 0; 
  sv_int_t led;
  initialize_int(&led);
  led.value = 0;
  
  //PRINT("forking myfib\r\n");
  //fork_routine( (act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 4, &r) );

  PRINT("forking blinky\r\n");
  fork_routine( (act_t *) enter_main(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, &led) );
  

  
  PRINT("Waiting for ticks\r\n");
  int i = 0; 
  while (1) {
    recv_msg.size = 0;
    recv_msg.rx_source_thread = K_ANY;

    //PRINT("step[%d]: r = %d\r\n", i, r.value); 
    /* PRINT("now: %llu\r\n", now); */
    /* PRINT("next: %llu\r\n",next_event_time()); */
    /* PRINT("Events queued: %d\r\n", event_queue_len); */

    /* PRINT("step[%d]: led = %d\r\n", i, led.value); */
    /* PRINT("now: %llu\r\n", now); */
    /* PRINT("next: %llu\r\n",next_event_time()); */
    /* PRINT("Events queued: %d\r\n", event_queue_len); */
  
    
    /* get a data item, waiting as long as needed */
    k_mbox_get(&tick_mbox, &recv_msg, NULL, K_FOREVER);
 
    now = next_event_time();
    tick();

    uint64_t sleep_time = next_event_time() - now; /* in milliseconds */

    //PRINT("sleep_time = %lld\r\n", sleep_time);
    
    alarm_cfg.ticks = counter_us_to_ticks(counter_dev, 1000*(uint32_t)sleep_time); /* ms */
  
    if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
      //PRINT("hw_tick: Alarm set\r\n");
    } else {
      PRINT("hw_tick: Error setting alarm\r\n");
    }
        
    i++;
  }
}

void start_tick_thread(void) {

  k_thread_create(&tick_thread, tick_thread_stack,
		  K_THREAD_STACK_SIZEOF(tick_thread_stack),
		  tick_thread_main,
		  NULL, NULL, NULL,
		  5, 0, K_NO_WAIT);
}

/* ************************************************************ */
/* MAIN                                                         */
/* ************************************************************ */

void main(void) {
  
  /* *************  */ 
  /* Start USB_CDC  */
  //  start_usb_cdc_thread();

  //PRINT("USB_CDC: started\r\n");
  PRINT("Sleeping 1 seconds\r\n");
  k_sleep(K_SECONDS(1)); // Wait enough for starting up a terminal.
  PRINT("WOKE UP\r\n");

  PRINT("Init LEDs\r\n");
  if (!led_init()) {
    PRINT("ERROR INIT LEDs\r\n");
    return;
  }


  
  /* ********** */
  /* MESSAGEBOX */
  PRINT("Creating messagebox\n");
  k_mbox_init(&tick_mbox);
  

  
  /* ************************* */
  /* Hardware timer experiment */
  PRINT("Configuring hardware timer \r\n");
  counter_dev = device_get_binding(TIMER);
  if (!counter_dev) {
    PRINT("HWCounter: Device not found error\r\n");
  }

  counter_start(counter_dev);

  alarm_cfg.flags = 0;
  alarm_cfg.ticks = counter_us_to_ticks(counter_dev, 10000);
  alarm_cfg.callback = hw_tick;
  alarm_cfg.user_data = &alarm_cfg;

  if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) {
    PRINT("HWCounter: Alarm set\r\n");
  } else {
    PRINT("HWCounter: Error setting alarm\r\n");
  }

  /* configure uart */

 
  if (ll_uart_init(&uart_drv, UART0, &uart0, uart0_in_buffer, 1024, uart0_out_buffer, 1024)) {
    PRINT("LL_UART: OK!\r\n");
  } else {
    PRINT("LL_UART: Failed!\r\n");
  }


  PRINT("Starting Tick-Thread\r\n");
  start_tick_thread();

  int led1_state = 0;
  
  while(1) {
    set_led(1,led1_state);
    led1_state = 1 - led1_state;
    k_sleep(K_SECONDS(1));
  }
}
