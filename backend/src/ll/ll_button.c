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

#include <ll_button.h>
#include <button.h>

static uint32_t ll_button_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

static uint32_t ll_button_data_available(struct ll_driver_s *this) {
  return 1;
}

static uint32_t ll_button_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  button_driver_t *b = (button_driver_t*)this->driver_info;

  uint32_t r = 0;
  
  if (data_size == 4) {
    data[0] = b->state;
    data[1] = b->state >> 8;
    data[2] = b->state >> 16;
    data[3] = b->state >> 24;
    r = 4;
  }
  
  return r;
}

static uint32_t ll_button_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

bool ll_button_init(ll_driver_t* lld, uint32_t drv_id, void* backend_custom,  uint32_t button_id) {

  button_driver_t *button_driver = button_init(drv_id, backend_custom, button_id);

  bool r = false;
  
  if (button_driver) {
    r = true; 
    lld->driver_info = (void*) button_driver;
    lld->ll_control_fun = ll_button_control;
    lld->ll_read_fun = ll_button_read;
    lld->ll_write_fun = ll_button_write;
    lld->ll_data_readable_fun = ll_button_data_available;
  }
  return r;
}



  
