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

#include <led.h>

#include <ll/ll_led.h>
#include <string.h>

static uint32_t ll_led_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

static uint32_t ll_led_data_available(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

static uint32_t ll_led_data_writeable(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

/* sets bit 0 in the first byte of data to the value of the led.
    The rest of data is cleared. */ 
static uint32_t ll_led_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_led_driver_t *led_driver = (ll_led_driver_t*)this->driver_info;
  bool  state = led_driver->internal.state;

  if (data_size > 0) {
    memset(data,0, data_size);
    data[0] = state?1:0;
  }
  return data_size;
}

/* data[0] will be reinterpreted as a bool */
static uint32_t ll_led_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_led_driver_t *led_driver = (ll_led_driver_t*)this->driver_info;
  if (data_size > 0) {
    palWritePad(led_driver->internal.port, led_driver->internal.pad, data[0]);\
    led_driver->internal.state = data[0];  
  }
  return data_size; /* there are alternative interpretations one could make... */
}

bool ll_led_init(ll_driver_t* lld, ll_led_driver_t *ldrv){

  lld->driver_info = ldrv;
  lld->is_synchronous = true;
  lld->ll_control_fun = ll_led_control;
  lld->ll_read_fun = ll_led_read;
  lld->ll_write_fun = ll_led_write;
  lld->ll_data_readable_fun = ll_led_data_available;
  lld->ll_data_writeable_fun = ll_led_data_writeable;

  return true;
}

