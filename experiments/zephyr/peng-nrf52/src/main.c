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

/* Our own library of stuff! */
//#include "defines.h"
#include "usb_cdc.h"
//#include "ltr_303als.h"
//#include "bme280.h"
#include "uart.h"
#include "ll_uart.h"
//#include "powerman.h"

//#define PRINT usb_printf
#define PRINT printk

/* LEDS */

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

/* ********** */
/*   TIMER    */

#define TIMER DT_LABEL(DT_NODELABEL(timer4))
struct  counter_alarm_cfg alarm_cfg;



/* *************** */
/*   UART buffers  */

ll_driver_t uart_drv;
uart_dev_t uart0;

uint8_t uart0_in_buffer[1024];
uint8_t uart0_out_buffer[1024];

/* ************************************************************ */
/* PENG STUFF                                                   */
/* ************************************************************ */


/* typedef struct { */
/*     /\* Procedure generic fields *\/; */
/*   ACTIVATION_RECORD_FIELDS; */
/*     /\* void (*step)(act_t *); *\/ */
/*     /\* uint16_t  pc; *\/ */
/*     /\* act_t    *caller; *\/ */
/*     /\* uint16_t  children; *\/ */
/*     /\* uint32_t  priority; *\/ */
/*     /\* uint8_t   depth; *\/ */
/*     /\* bool      scheduled; *\/ */
/*     /\* Procedure specific fields *\/; */
/*     sv_int_t *r; // Procedure argument; */
/*     trigger_t trig1; */
/* } act_mywait_t; */

/* act_mywait_t *enter_mywait(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r); */
/* void step_mywait(act_t *gen_act); */
/* act_mywait_t *enter_mywait(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r) { */
/*     act_mywait_t *act = (act_mywait_t *) enter(sizeof(act_mywait_t), step_mywait, caller, priority, depth); */
/*     act->r = r; */
/*     act->trig1.act = (act_t *) act; */
/*     return act; */
/* }; */

/* void step_mywait(act_t *gen_act) { */
/*     act_mywait_t *act = (act_mywait_t *) gen_act; */
/*     switch(act->pc) { */
/*         case 0: */
/*             sensitize((sv_t *) act->r, &act->trig1); */
/*             act->pc = 1; */
/*             return; */

/*         case 1: */
/*             desensitize(&act->trig1); */

/*         leave((act_t *) act, sizeof(act_mywait_t)); */
/*     } */
/* } */

/* typedef struct { */
/*     /\* Procedure generic fields *\/; */
/*   ACTIVATION_RECORD_FIELDS; */
/*     /\* void (*step)(act_t *); *\/ */
/*     /\* uint16_t  pc; *\/ */
/*     /\* act_t    *caller; *\/ */
/*     /\* uint16_t  children; *\/ */
/*     /\* uint32_t  priority; *\/ */
/*     /\* uint8_t   depth; *\/ */
/*     /\* bool      scheduled; *\/ */
/*     /\* Procedure specific fields *\/; */
/*     sv_int_t *r1; // Procedure argument; */
/*     sv_int_t *r2; // Procedure argument; */
/*     sv_int_t *r; // Procedure argument; */
/*     sv_int_t v1; // Declared at line 16, column 5 in file Fib.hs; */
/*     sv_int_t v2; // Declared at line 17, column 5 in file Fib.hs; */
/* } act_mysum_t; */

/* act_mysum_t *enter_mysum(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r1, sv_int_t *r2, sv_int_t *r); */
/* void step_mysum(act_t *gen_act); */
/* act_mysum_t *enter_mysum(act_t *caller, uint32_t priority, uint8_t depth, sv_int_t *r1, sv_int_t *r2, sv_int_t *r) { */
/*     act_mysum_t *act = (act_mysum_t *) enter(sizeof(act_mysum_t), step_mysum, caller, priority, depth); */
/*     act->r1 = r1; */
/*     act->r2 = r2; */
/*     act->r = r; */
/*     return act; */
/* }; */

/* void step_mysum(act_t *gen_act) { */
/*     act_mysum_t *act = (act_mysum_t *) gen_act; */
/*     switch(act->pc) { */
/*         case 0: */
/*             { */
/*             uint8_t new_depth = act->depth - 1; */
/*             uint32_t pinc = 1 << new_depth; */
/*             uint32_t new_priority = act->priority; */
/*             fork_routine((act_t *) enter_mywait( (act_t *) act */
/*                                                , new_priority */
/*                                                , new_depth */
/*                                                , act->r1)); */
/*             new_priority += pinc; */
/*             fork_routine((act_t *) enter_mywait( (act_t *) act */
/*                                                , new_priority */
/*                                                , new_depth */
/*                                                , act->r2)); */
/*             } */
/*             act->pc = 1; */
/*             return; */

/*         case 1: */
/*             assign_int(&act->v1, act->priority, act->r1->value); */
/*             assign_int(&act->v2, act->priority, act->r2->value); */
/*             later_int(act->r, now + 1, (act->v1.value) + (act->v2.value)); */

/* 	leave((act_t *) act, sizeof(act_mysum_t)); */
/*     } */
/* } */

/* typedef struct { */
/*     /\* Procedure generic fields *\/; */
/*    ACTIVATION_RECORD_FIELDS; */
/*   //  void (*step)(act_t *); */
/*   //  uint16_t  pc; */
/*   //  act_t    *caller; */
/*   //  uint16_t  children; */
/*   //  uint32_t  priority; */
/*   //  uint8_t   depth; */
/*   //  bool      scheduled; */
/*     /\* Procedure specific fields *\/; */
/*     sv_int_t n; // Procedure argument; */
/*     sv_int_t *r; // Procedure argument; */
/*     sv_int_t *r1; // Declared at line 22, column 5 in file Fib.hs; */
/*     sv_int_t *r2; // Declared at line 23, column 5 in file Fib.hs; */
/* } act_myfib_t; */

/* act_myfib_t *enter_myfib(act_t *caller, uint32_t priority, uint8_t depth, int n, sv_int_t *r); */
/* void step_myfib(act_t *gen_act); */


/* sv_int_t r1_static; */
/* sv_int_t r2_static; */

/* act_myfib_t *enter_myfib(act_t *caller, uint32_t priority, uint8_t depth, int n, sv_int_t *r) { */
/*     act_myfib_t *act = (act_myfib_t *) enter(sizeof(act_myfib_t), step_myfib, caller, priority, depth); */
/*     initialize_int(&act->n); */
/*     assign_int(&act->n, act->priority, n); */
/*     act->r = r; */
/*     //PRINT("enter_myfib: Allocating space for two sv_int\r\n"); */
/*     act->r1 = (sv_int_t *) malloc(sizeof(sv_int_t)); */
/*     act->r2 = (sv_int_t *) malloc(sizeof(sv_int_t)); */
/*     //PRINT("enter_myfib: returning\r\n"); */
/*     return act; */
/* }; */

/* int steps = 0;  */

/* void step_myfib(act_t *gen_act) { */

/*   //PRINT("step_myfib\r\n"); */
  
/*   steps ++;  */
/*     act_myfib_t *act = (act_myfib_t *) gen_act; */
/*     switch(act->pc) { */
/*         case 0: */
/*             initialize_int(act->r1); */
/*             assign_int(act->r1, act->priority, 0); */
/*             initialize_int(act->r2); */
/*             assign_int(act->r2, act->priority, 0); */
/*             if (!((act->n.value) < (2))) goto L0; */
/*             later_int(act->r, now + 1, 1); */
/*             goto L1; */

/*         L0: */
/*             { */
/*             uint8_t new_depth = act->depth - 1; */
/*             uint32_t pinc = 1 << new_depth; */
/*             uint32_t new_priority = act->priority; */
/*             fork_routine((act_t *) enter_myfib( (act_t *) act */
/*                                               , new_priority */
/*                                               , new_depth */
/*                                               , (act->n.value) - (1) */
/*                                               , act->r1)); */
/*             new_priority += pinc; */
/*             fork_routine((act_t *) enter_myfib( (act_t *) act */
/*                                               , new_priority */
/*                                               , new_depth */
/*                                               , (act->n.value) - (2) */
/*                                               , act->r2)); */
/*             new_priority += pinc; */
/*             fork_routine((act_t *) enter_mysum( (act_t *) act */
/*                                               , new_priority */
/*                                               , new_depth */
/*                                               , act->r1 */
/*                                               , act->r2 */
/*                                               , act->r)); */
/*             } */
/*             act->pc = 1; */
/* 	    //PRINT("step_myfib: r1 = %d | r2 = %d | r = %d \r\n",act->r1->value,act->r2->value,act->r->value); */
/* 	    //uart_printf(&uart0,"r1 = %d | r2 = %d | r = %d \r\n",act->r1->value,act->r2->value,act->r->value); */
/*             return; */

/*     case 1: */
      

/*     L1: */
/*       PRINT("LEAVING\r\n"); */
/*       free(act->r1); */
/*       free(act->r2); */
/*       leave((act_t *) act, sizeof(act_myfib_t)); */
/*     } */
/* } */


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

  struct counter_alarm_cfg *config = user_data;
  //uint32_t now_ticks;
  //if (!counter_get_value(dev, &now_ticks)) {
  //  uint32_t now_usec = counter_ticks_to_us(dev, now_ticks);
  //  int now_sec = (int)(now_usec / USEC_PER_SEC);
  //} else {
   
  //}

  //uint64_t t_now = now;
  //uint64_t t_next = next_event_time();

  //config->ticks = counter_us_to_ticks(dev, t_next - t_now);
  //PRINT("Sleeping for %uus\r\n",counter_us_to_ticks(dev, t_next - t_now));

  /* Put a tick on the messagebox */ 
  struct k_mbox_msg send_msg;
  
  send_msg.info = 101;
  send_msg.size = 0;
  send_msg.tx_data = NULL;
  send_msg.tx_target_thread = K_ANY;

  /* third argument in async put is a semaphore  
     that can be set to NULL if not "needed" _*/
  k_mbox_async_put(&tick_mbox, &send_msg, NULL);

  config->ticks = counter_us_to_ticks(dev, 100000);
  
  if (!counter_set_channel_alarm(dev, 0, config)) {
    //PRINT("hw_tick: Alarm set\r\n");
  } else {
    PRINT("hw_tick: Error setting alarm\r\n");
  }
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

  PRINT("forking myfib\r\n");
  fork_routine( (act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 8, &r) );

  PRINT("Waiting for ticks\r\n");
  int i = 0; 
  while (1) {
    recv_msg.size = 0;
    recv_msg.rx_source_thread = K_ANY;

    PRINT("step[%d]: r = %d\n", i, r.value);
    PRINT("now: %12lu\n", (uint32_t)now);
    PRINT("Events queued: %d\n", event_queue_len);
    
    /* get a data item, waiting as long as needed */
    k_mbox_get(&tick_mbox, &recv_msg, NULL, K_FOREVER);
 
    now = next_event_time();
    tick();
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
  /* ******************* */
  /* Configure some LEDs */
  
  const struct device *d_led0;
  const struct device *d_led1;

  d_led0 = device_get_binding(LED_DEVICE_LABEL(led0));
  d_led1 = device_get_binding(LED_DEVICE_LABEL(led1));
  gpio_pin_configure(d_led0, LED_PIN(led0), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led0));
  gpio_pin_configure(d_led1, LED_PIN(led1), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led1));
  gpio_pin_set(d_led0, LED_PIN(led0), 0);
  gpio_pin_set(d_led1, LED_PIN(led1), 0);

  /* ********** */
  /* MESSAGEBOX */
  PRINT("Creating messagebox\n");
  k_mbox_init(&tick_mbox);
  

  
  /* ************************* */
  /* Hardware timer experiment */
  PRINT("Configuring hardware timer \r\n");
  const struct device *counter_dev = device_get_binding(TIMER);
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


  
  int led0_state = 1;
  int led1_state = 0;
  
  while(1) {
    gpio_pin_set(d_led0, LED_PIN(led0), led0_state);
    gpio_pin_set(d_led1, LED_PIN(led1), led1_state);
    led0_state = 1 - led0_state;
    led1_state = 1 - led1_state;
    k_sleep(K_MSEC(500));
  }
}
