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

/* GPIOS */
#define PSM_IND    1
#define PON_TRIG   5
#define PWR_KEY    6
#define RESET      7


/*******************/
/* UART 2 UART 5   */


const struct device *uart2;
const struct device *uart5;

struct uart_buffers {
  struct ring_buf in_ringbuf;
  struct ring_buf out_ringbuf;
}; 

uint8_t uart2_in_buffer[1024];
uint8_t uart2_out_buffer[1024];

uint8_t uart5_in_buffer[1024];
uint8_t uart5_out_buffer[1024];

struct uart_buffers uart2_buffers;
struct uart_buffers uart5_buffers;

volatile int slen;
volatile int dropped;


static void uart_isr(const struct device *dev, void *args)
{
  struct uart_buffers *bufs = (struct uart_buffers*)args;
  
  while (uart_irq_update(dev) && uart_irq_is_pending(dev)) {
    if (uart_irq_rx_ready(dev)) {
      int recv_len, rb_len;
      uint8_t buffer[64];
      size_t len = MIN(ring_buf_space_get(&bufs->in_ringbuf),
		       sizeof(buffer));
      
      recv_len = uart_fifo_read(dev, buffer, len);

      rb_len = ring_buf_put(&bufs->in_ringbuf, buffer, recv_len);
      if (rb_len < recv_len) {
	//silently dropping bytes
      } 
    }

    if (uart_irq_tx_ready(dev)) {
      uint8_t c;
      int rb_len, send_len;
      
      rb_len = ring_buf_get(&bufs->out_ringbuf, &c, 1);
      if (rb_len == 0) {
	uart_irq_tx_disable(dev);
	continue;
      }

      send_len = uart_fifo_fill(dev, &c, 1);
      slen = send_len;
      if (send_len < rb_len) {
	dropped = rb_len - send_len;
	//LOG_ERR("Drop %d bytes", rb_len - send_len);
      }
    }
  }
}

int get_char(struct uart_buffers *buffs) {

  int n;
  uint8_t c;
  unsigned int key = irq_lock();
  n = ring_buf_get(&buffs->in_ringbuf, &c, 1);
  irq_unlock(key);
  if (n == 1) {
    return c;
  }
  return -1;
}

int put_char(struct uart_buffers *buffs, char c) {

  int n = 0;
  unsigned int key = irq_lock();
  n = ring_buf_put(&buffs->out_ringbuf, &c, 1);
  irq_unlock(key);
  return n;
}

void uart_printf(struct uart_buffers *buffs, char *format, ...) {

  va_list arg;
  va_start(arg, format);
  int len;
  static char print_buffer[4096];

  len = vsnprintf(print_buffer, 4096,format, arg);
  va_end(arg);

  int num_written = 0;
  while (len - num_written > 0) {
    unsigned int key = irq_lock();
    num_written +=
      ring_buf_put(&buffs->out_ringbuf,
		   (print_buffer + num_written),
		   (len - num_written));
    irq_unlock(key);
    PRINT("BASE: num_written %d\r\n", num_written);
    uart_irq_tx_enable(uart2);
  }

  PRINT("BASE: last send_len %d\r\n", slen);
  PRINT("BASE: last dropped %d\r\n", dropped);

}


/************/
void main(void)
{
  uint32_t baudrate;
  int r;
  
  start_usb_cdc_thread();

  k_sleep(K_SECONDS(4));
  
  const struct device *d_led0;
  const struct device *d_led1;
  
  d_led0 = device_get_binding(LED_DEVICE_LABEL(led0));
  d_led1 = device_get_binding(LED_DEVICE_LABEL(led1));
  gpio_pin_configure(d_led0, LED_PIN(led0), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led0));
  gpio_pin_configure(d_led1, LED_PIN(led1), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led1));

  gpio_pin_set(d_led0, LED_PIN(led0), 0);
  gpio_pin_set(d_led1, LED_PIN(led1), 0);
  
  int led0_state = 0;
  int led1_state = 1;

  /* configure gpio pins */
  const struct device *d_gpio_b;

  d_gpio_b = device_get_binding("GPIOB");

  if (!d_gpio_b) {
    PRINT("GPIO_B: Error no binding\r\n");
    return; 
  } else {
    PRINT("GPIO_B: Success\r\n");
  }

  gpio_pin_configure(d_gpio_b, PSM_IND, GPIO_INPUT);
  gpio_pin_configure(d_gpio_b, PON_TRIG, GPIO_OUTPUT_LOW);
  gpio_pin_configure(d_gpio_b, PWR_KEY, GPIO_OUTPUT_LOW);
  gpio_pin_configure(d_gpio_b, RESET, GPIO_OUTPUT_LOW);

  gpio_pin_set(d_gpio_b, PON_TRIG, 1);
  
  /* configure uart */

  ring_buf_init(&uart2_buffers.in_ringbuf, 1024, uart2_in_buffer);
  ring_buf_init(&uart2_buffers.out_ringbuf, 1024, uart2_out_buffer);

  ring_buf_init(&uart5_buffers.in_ringbuf, 1024, uart5_in_buffer);
  ring_buf_init(&uart5_buffers.out_ringbuf, 1024, uart5_out_buffer);

  
  PRINT("Configuring UART2\r\n");
  uart2 = device_get_binding("UART_2");
  if (!uart2) {
    PRINT("UART2: Device binding FAILED!\r\n");
    return;
  }


  PRINT("Configuring UART5\r\n");
  uart5 = device_get_binding("UART_5");
  if (!uart5) {
    PRINT("UART5: Device binding FAILED!\r\n");
    return;
  }  
  
  uart_irq_callback_user_data_set(uart2, uart_isr, (void*)&uart2_buffers);
  uart_irq_rx_enable(uart2);
  PRINT("UART2: ISR activated\r\n");

  uart_irq_callback_user_data_set(uart5, uart_isr, (void*)&uart5_buffers);
  uart_irq_rx_enable(uart5);
  PRINT("UART5: ISR activated\r\n");


  gpio_pin_set(d_led0, LED_PIN(led0), 1);

  char cmd_buffer[1024];
  while(1) {

    int c;
    memset(cmd_buffer,0,1024);
    if (usb_has_data()) {

      PRINT("USB: Has data waiting for end of line\r\n");
      if (usb_readl(cmd_buffer, 1024) >= 0) {
	PRINT("USB: recv %s\r\n", cmd_buffer);
	uart_printf(&uart2_buffers, "%s", cmd_buffer);
      }

      if ( strncmp(cmd_buffer, "PWR_KEY", 7) == 0 ) {
	k_sleep(K_MSEC(5000));
	gpio_pin_set(d_gpio_b, PWR_KEY, 1);
	k_sleep(K_MSEC(750));
	gpio_pin_set(d_gpio_b, PWR_KEY, 0);
      }
    }
    

    PRINT("COMM_UART:");
    while ((c = get_char(&uart2_buffers)) != -1 ) { 
       PRINT("%c", (char)c);
       gpio_pin_set(d_led1, LED_PIN(led1),1);
    }
    PRINT("\r\n");

    PRINT("DBG_UART: ");
    while ((c = get_char(&uart5_buffers)) != -1 ) { 
       PRINT("%c", (char)c);
       gpio_pin_set(d_led1, LED_PIN(led1),1);
    }
    PRINT("\r\n");
    
    //gpio_pin_set(d_led0, LED_PIN(led0), led0_state);
    //


    k_sleep(K_MSEC(1000));
    gpio_pin_set(d_led1, LED_PIN(led1),0);

    //led1_state = 1 - led1_state;
    //led0_state = 1 - led0_state;    
  }
}
