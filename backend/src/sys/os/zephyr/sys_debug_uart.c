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


#include <sys/sys_debug_uart.h> /* sensevm sys */
#include <devicetree.h>
#include <drivers/uart.h>
#include <sys/ring_buffer.h>     /* zephyr sys - crap */

#if DT_NODE_HAS_STATUS(DT_ALIAS(svm_debug_uart), okay)

static const struct device* dev;
static const struct debug_in_ringbuf;
static const struct debug_out_ringbuf;

/* ************************* */
/* Interrupt service routine */

static void debug_uart_isr(const struct device *dev, void *args)
{
  uart_dev_t *bufs = (uart_dev_t*)args;

  while (uart_irq_update(dev) && uart_irq_is_pending(dev)) {
    if (uart_irq_rx_ready(dev)) {
      int recv_len, rb_len;
      uint8_t buffer[64];
      size_t len = MIN(ring_buf_space_get(&debug_in_ringbuf),
		       sizeof(buffer));

      recv_len = uart_fifo_read(dev, buffer, len);

      rb_len = ring_buf_put(&debug_in_ringbuf, buffer, recv_len);
      if (rb_len < recv_len) {
	//silently dropping bytes
      }
    }

    if (uart_irq_tx_ready(dev)) {
      uint8_t c;
      int rb_len, send_len;

      rb_len = ring_buf_get(&debug_out_ringbuf, &c, 1);
      if (!rb_len) {
	uart_irq_tx_disable(dev);
	continue;
      }

      send_len = uart_fifo_fill(dev, &c, 1);
    }
  }
}




#else

void debug_uart_init(uint32_t buffer_size) {
  return;
}

int debug_printf(const char* format, ...) {
  return 0;
}

#endif
