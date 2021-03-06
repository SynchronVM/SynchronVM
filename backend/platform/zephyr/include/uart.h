/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar 				  */
/* 										  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy	  */
/* of this software and associated documentation files (the "Software"), to deal  */
/* in the Software without restriction, including without limitation the rights	  */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell	  */
/* copies of the Software, and to permit persons to whom the Software is	  */
/* furnished to do so, subject to the following conditions:			  */
/* 										  */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software.				  */
/* 										  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR	  */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,	  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE	  */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER	  */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  */
/* SOFTWARE.									  */
/**********************************************************************************/

#ifndef UART_H_
#define UART_H_

#include <stdint.h>
#include <sys/ring_buffer.h>
#include <drivers/uart.h>
#include <svm_zephyr.h>

#include <ll/ll_uart.h>

typedef struct {
  struct ring_buf in_ringbuf;
  struct ring_buf out_ringbuf;
  const struct device *dev;
  zephyr_interop_t* interop;
} uart_dev_t;

extern uart_dev_t* uart_init(ll_uart_if_t uif,
			     uint8_t *in_buffer,
			     uint32_t in_size,
			     uint8_t *out_buffer,
			     uint32_t out_size,
			     void *backend_custom);
extern bool uart_get_baudrate(uart_dev_t *u, uint32_t *baud);
extern bool uart_data_available(uart_dev_t *dev);
extern uint32_t uart_ndata_available(uart_dev_t *dev);
extern uint32_t uart_ndata_writeable(uart_dev_t *dev);
extern int uart_get_char(uart_dev_t *buffs);
extern int uart_put_char(uart_dev_t *buffs, char c);
extern uint32_t uart_read_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size);
extern uint32_t uart_write_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size);
extern void uart_printf(uart_dev_t *buffs, char *format, ...);

#endif
