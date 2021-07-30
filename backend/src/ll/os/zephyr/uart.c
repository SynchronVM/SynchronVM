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

#include <uart.h>
#include <stdio.h>
#include <stdint.h>

#include <devicetree.h>

/* TODO: Surely there are problems with this
         if multiple os-threads read and write
	 using the same uart device (uart_dev_t) */  

/* ************************* */
/* UARTS                     */

uart_dev_t uarts[8];


/* ************************* */
/* Interrupt service routine */

static void uart_isr(const struct device *dev, void *args)
{
  uart_dev_t *bufs = (uart_dev_t*)args;

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
      if (!rb_len) {
	uart_irq_tx_disable(dev);
	continue;
      }

      send_len = uart_fifo_fill(dev, &c, 1);
    }
  }
}

/* ******************************** */
/* Initialization and configuration */
uart_dev_t* uart_init(ll_uart_if_t uif,
		      uint8_t *in_buffer,
		      uint32_t in_size,
		      uint8_t *out_buffer,
		      uint32_t out_size,
		      void *backend_custom) {
  int uart_id = -1;
  switch(uif) {
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart0), okay)    
  case UART_IF0:
    uarts[0].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart0)));
    uart_id = 0;
    break;  
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart1), okay)
  case UART_IF1:
    uarts[1].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart1)));
    uart_id = 1;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart2), okay)
  case UART_IF2:
    uarts[2].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart2)));
    uart_id = 2;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart3), okay)
  case UART_IF3:
    uarts[3].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart3)));
    uart_id = 3;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart4), okay)
  case UART_IF4:
    uarts[4].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart4)));
    uart_id = 4;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart5), okay)
  case UART_IF5:
    uarts[5].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart5)));
    uart_id = 5;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart6), okay)
  case UART_IF6:
    uarts[6].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart6)));
    uart_id = 6;
    break;
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_uart7), okay)
  case UART_IF7:
    uarts[7].dev = device_get_binding(DT_LABEL(DT_ALIAS(svm_uart7)));
    uart_id = 7;
    break;
#endif
  default:
    uart_id = -1;
    break;
  }

  if (uart_id >= 0 && uart_id <= 7) {

    ring_buf_init(&uarts[uart_id].in_ringbuf, in_size, in_buffer);
    ring_buf_init(&uarts[uart_id].out_ringbuf, out_size, out_buffer);
    
    uart_irq_callback_user_data_set(uarts[uart_id].dev, uart_isr, (void*)&uarts[uart_id]);
    uart_irq_rx_enable(uarts[uart_id].dev);

    uarts[uart_id].interop = (zephyr_interop_t*)backend_custom;
    
    return &uarts[uart_id];
  }

  return NULL;
}

bool uart_get_baudrate(uart_dev_t *u, uint32_t *baud) {
  return (bool)uart_line_ctrl_get(u->dev, UART_LINE_CTRL_BAUD_RATE, baud);
}

/* ************************************* */
/* Are there bytes ?                     */

bool uart_data_available(uart_dev_t *dev) {
  return !ring_buf_is_empty(&dev->out_ringbuf);  
}

uint32_t uart_ndata_available(uart_dev_t *dev) {
  return
    ring_buf_capacity_get(&dev->out_ringbuf) -
    ring_buf_space_get(&dev->out_ringbuf);
}

uint32_t uart_ndata_writeable(uart_dev_t *dev) {
  return ring_buf_space_get(&dev->in_ringbuf);
}



/* ************************************* */
/* Printing to and reading from the UART */

int uart_get_char(uart_dev_t *buffs) {

  int n;
  uint8_t c;
  unsigned int key = irq_lock(); /* disables all interrupts */
                                 /* TODO: Maybe it is possible to specifically just turn 
				    of the uart interrupt in question? */
  n = ring_buf_get(&buffs->in_ringbuf, &c, 1);
  irq_unlock(key);
  if (n == 1) {
    return c;
  }
  return -1;
}

int uart_put_char(uart_dev_t *buffs, char c) {

  int n = 0;
  unsigned int key = irq_lock();
  n = ring_buf_put(&buffs->out_ringbuf, &c, 1);
  irq_unlock(key);
  uart_irq_tx_enable(buffs->dev);
  return n;
}

uint32_t uart_read_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size) {

  if (data_size > (ring_buf_capacity_get(&dev->in_ringbuf) -
		   ring_buf_space_get(&dev->in_ringbuf))) return 0;
  
  unsigned int key = irq_lock();
  uint32_t n = ring_buf_get(&dev->in_ringbuf, data, data_size);
  irq_unlock(key);
  return n;
}

uint32_t uart_write_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size) {

  uint32_t free_space = ring_buf_space_get(&dev->out_ringbuf);
  if (free_space < data_size) return 0;
  unsigned int key = irq_lock();
  uint32_t n = ring_buf_put(&dev->out_ringbuf, data, data_size);
  irq_unlock(key);
  uart_irq_tx_enable(dev->dev);
  return n;
}

void uart_printf(uart_dev_t *buffs, char *format, ...) {

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
    uart_irq_tx_enable(buffs->dev);
  }

}
