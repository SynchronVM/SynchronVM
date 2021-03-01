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

#include <ll_uart.h>

static bool ll_uart_data_available(struct ll_driver_s *this) {
  return uart_data_available((uart_dev_t*)this->driver_info);
}

static int ll_uart_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return uart_read_bytes((uart_dev_t*)this->driver_info, data, data_size);
}

static int ll_uart_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return uart_write_bytes((uart_dev_t*)this->driver_info, data, data_size);
}


bool ll_uart_init(ll_driver_t* lld, uart_if_t uif, uart_dev_t *u,
		  uint8_t *in_buffer,
		  uint32_t in_size,
		  uint8_t *out_buffer,
		  uint32_t out_size) {

  bool r = uart_init(uif, u, in_buffer, in_size, out_buffer, out_size);
  if (r) { 
    lld->driver_info = (void*) u; /* store the uart device as driver internal info */
    lld->ll_read_fun = ll_uart_read;
    lld->ll_write_fun = ll_uart_write;
  }
  return r; 
}


