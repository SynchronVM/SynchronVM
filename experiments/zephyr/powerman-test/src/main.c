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
#include "usb_cdc.h"
#include "ltr_303als.h"
#include "bme280.h"
//#include "uart.h"
#include "ll_uart.h"
#include "powerman.h"


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


/* void tick_fun(struct k_timer *timer_id) { */
/*   PRINT("System clock: TICK\r\n"); */
/* } */


/* void hw_tick(const struct device *dev, uint8_t chan, uint32_t ticks, void *user_data) { */

/*   struct counter_alarm_cfg *config = user_data; */
/*   uint32_t now_ticks; */
/*   if (!counter_get_value(dev, &now_ticks)) { */
/*     PRINT("hw_tick: now_ticks = %u\r\n", now_ticks); */
/*     uint32_t now_usec = counter_ticks_to_us(dev, now_ticks); */
/*     int now_sec = (int)(now_usec / USEC_PER_SEC); */
/*   } else { */
/*     PRINT("hw_tick: Error getting now_ticks\r\n"); */
/*   } */

/*   config->ticks = 16000000; */

/*   if (!counter_set_channel_alarm(dev, 0, config)) { */
/*     //PRINT("hw_tick: Alarm set\r\n"); */
/*   } else { */
/*     PRINT("hw_tick: Error setting alarm\r\n"); */
/*   } */

/* } */


void main(void) {
  
  /* *************  */ 
  /* Start USB_CDC  */
  //  start_usb_cdc_thread();

  //PRINT("USB_CDC: started\r\n"); 
  k_sleep(K_SECONDS(5));

  
  /* ***************** */
  /* Register powerman */ 
  powerman_init();

  PRINT("POWERMAN: started\r\n");
  
  k_sleep(K_SECONDS(5));

  k_thread_foreach(t_info_dump, NULL);

  PRINT("t_info_dump called %d times\r\n", t_counter);

  PRINT("Going to sleep 30 seconds\r\n");
  k_sleep(K_SECONDS(30));
  PRINT("Woke up!\r\n");

  
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

  /* ************************************* */
  /* System clock based timer "interrupts" */
  /* PRINT("Configuring k_timer tick_fun\r\n"); */
  /* struct k_timer my_timer; */

  /* k_timer_init(&my_timer, tick_fun, NULL); */

  /* k_timer_start(&my_timer, K_MSEC(500), K_MSEC(500)); */

  /* ************************* */
  /* Hardware timer experiment */
  /* PRINT("Configuring hardware timer \r\n"); */
  /* const struct device *counter_dev = device_get_binding(TIMER); */
  /* if (!counter_dev) { */
  /*   PRINT("HWCounter: Device not found error\r\n"); */
  /* } */

  /* counter_start(counter_dev); */

  /* alarm_cfg.flags = 0; */
  /* alarm_cfg.ticks = 16000000; //counter_us_to_ticks(counter_dev, 1000); */
  /* alarm_cfg.callback = hw_tick; */
  /* alarm_cfg.user_data = &alarm_cfg; */

  /* if (!counter_set_channel_alarm(counter_dev, 0, &alarm_cfg)) { */
  /*   PRINT("HWCounter: Alarm set\r\n"); */
  /* } else { */
  /*   PRINT("HWCounter: Error setting alarm\r\n"); */
  /* } */


  PRINT("Configuring sensors\r\n");
  /* configure ltr-303als */
  if (!als_init()) {
    PRINT("ALS: Unable to initialize\r\n");
  }

  if (!als_set_gain(1)) {
    PRINT("ALS: Unable to set gain\r\n");
  }

  int counter  = 0;
  while (counter < 5) {

    uint16_t ch0 = 0;
    uint16_t ch1 = 0;

    if (als_read_data(&ch0,&ch1)) {
      PRINT("ALS CH0: %u\r\n", ch0);
      PRINT("ALS CH1: %u\r\n", ch1);
    } else {
      PRINT("ALS: Error reading data register");
    }
    k_sleep(K_SECONDS(1));
    counter ++;
  }

  /* BME280 */


  if (!bme_init()) {
    PRINT("BME280: Device not found\r\n");
    return;
  } else {
    PRINT("BME280: OK!\r\n");
  }

  while (counter < 10) {
    int32_t i, d;
    if (bme_sample()) {
      PRINT("--------------------------------\r\n");
      bme_get_temperature(&i, &d);
      PRINT("Temperature: %d.%06d\r\n", i, d);
      bme_get_pressure(&i,&d);
      PRINT("Pressure: %d.%06d\r\n", i, d);
      bme_get_humidity(&i, &d);
      PRINT("Humidity: %d.%06d\r\n", i, d);
    }
    k_sleep(K_SECONDS(1));
    counter++;
  }

  k_thread_foreach(t_info_dump, NULL);
  k_sleep(K_SECONDS(5));
  
  /* configure uart */

  /* ll_driver_t uart_drv; */
  /* uart_dev_t uart0; */

  /* if (ll_uart_init(&uart_drv, UART0, &uart0, uart0_in_buffer, 1024, uart0_out_buffer, 1024)) { */
  /*   PRINT("LL_UART: OK!\r\n"); */
  /* } else { */
  /*   PRINT("LL_UART: Failed!\r\n"); */
  /* } */

  /* const char *hello = "hello world\r\n"; */
  
  /* while (counter < 5) { */
  /*   ll_write(&uart_drv, hello, strlen(hello)); */
  /*   k_sleep(K_SECONDS(1)); */
  /*   counter++; */
  /* } */

  volatile int values[100];
  
  while (1) {

    k_sleep(K_SECONDS(10));
  
    for (int i = 0; i < 10; i ++) {
      gpio_pin_set(d_led1, LED_PIN(led1), i%2);

      for (int j = 0; j < 100; j ++) {
	values[j] = 0; 
      }
      
      for (int j = 0; j < 100; j ++) {
	for (int k = 0; k < 100000; k ++) {
	  values[j] += k; 
	}
      }
    }
    gpio_pin_set(d_led1, LED_PIN(led1), 0);
  }

}
