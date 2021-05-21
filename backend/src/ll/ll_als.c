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

#include <ll_als.h>


static uint32_t ll_als_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

static uint32_t ll_als_data_available(struct ll_driver_s *this) {
  return 4; // not really sure what to do here. the driver should be redesigned
}
static uint32_t ll_als_data_writeable(struct ll_driver_s *this) {
  return 0;
}

static uint32_t ll_als_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {

  bool r = als_read_data_sequential(data, data_size);

  
  return r ? data_size : 0 ; /* makes slightly more sense, But the error should be reported somehow. */ 
}

/* als does not accept any input */
static uint32_t ll_als_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0; 
}

bool ll_als_init(ll_driver_t* lld, uint8_t gain) {

  bool r = als_init();

  if (r) ll_driver_sleep_ms(ALS_STARTUP_TIME_MS);

  r = als_standby();
  
  if (r) ll_driver_sleep_ms(ALS_STANDBY_TIME_MS);
  
  r = als_activate(gain);

  if (r) {
    lld->driver_info = NULL;
    lld->ll_control_fun = ll_als_control;
    lld->ll_read_fun = ll_als_read;
    lld->ll_write_fun = ll_als_write;
    lld->ll_data_available_fun = ll_als_data_available;
    lld->ll_data_writeable_fun = ll_als_data_writeable;
  }

  if(r) ll_driver_sleep_ms(ALS_WAKEUP_TIME_MS);
  
  return r;

}
