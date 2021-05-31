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

#include <ll/ll_uart.h>


static uint32_t ll_uart_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return LL_DRIVER_CONTROL_SUCCESS;
}

static uint32_t ll_uart_data_available(struct ll_driver_s *this) {
  return uart_ndata_available((uart_dev_t*)this->driver_info);
}

static uint32_t ll_uart_data_writeable(struct ll_driver_s *this) {
  return uart_ndata_writeable((uart_dev_t*)this->driver_info);
}

static uint32_t ll_uart_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return uart_read_bytes((uart_dev_t*)this->driver_info, data, data_size);
}

static uint32_t ll_uart_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return uart_write_bytes((uart_dev_t*)this->driver_info, data, data_size);
}


bool ll_uart_init(ll_driver_t* lld,
		  uart_if_t uif,
		  uint8_t *in_buffer,
		  uint32_t in_size,
		  uint8_t *out_buffer,
		  uint32_t out_size,
		  void *backend_custom) {

  uart_dev_t* u = uart_init(uif, in_buffer, in_size, out_buffer, out_size, backend_custom);
  if (u) {
    lld->driver_info = (void*) u; /* store the uart device as ll_driver internal info */
    lld->ll_control_fun = ll_uart_control;
    lld->ll_read_fun = ll_uart_read;
    lld->ll_write_fun = ll_uart_write;
    lld->ll_data_readable_fun = ll_uart_data_available;
    lld->ll_data_writeable_fun = ll_uart_data_writeable;
    lld->ll_is_synchronous_fun = NULL;
  }
  return (bool)u;
}


