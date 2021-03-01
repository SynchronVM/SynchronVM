#ifndef UART_H_
#define UART_H_

#include <sys/ring_buffer.h>
#include <drivers/uart.h>

typedef enum { UART0 = 0, UART1, UART2, UART3, UART4, UART5, UART6, UART7 } uart_if_t; 			   
			  
typedef struct {
  struct ring_buf in_ringbuf;
  struct ring_buf out_ringbuf;
  const struct device *dev;
} uart_dev_t; 

extern bool uart_init(uart_if_t uif, uart_dev_t *u,
		      uint8_t *in_buffer,
		      uint32_t in_size,
		      uint8_t *out_buffer,
		      uint32_t out_size);
extern bool uart_get_baudrate(uart_dev_t *u, uint32_t *baud);
extern int uart_get_char(uart_dev_t *buffs);
extern int uart_put_char(uart_dev_t *buffs, char c);
extern void uart_printf(uart_dev_t *buffs, char *format, ...);

#endif
