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

#include <button.h>
#include <sys/sys_time.h>

#include <hal_pal.h>

void button_cb(void *arg) {

  chSysLockFromISR();
  
  button_driver_internal_t *button = (button_driver_internal_t *)arg;

  bool state = palReadPad(button->port, button->pad);

  button->state = state;
  
  svm_msg_t msg; 
  msg.sender_id = button->id; 
  msg.timestamp = 0; // TODO: implement
  msg.data = state;  // 1 or 0

  if (button->interop->send_message(button->interop, msg) == -1) {
    /* Message was not send due to queue being full. 
       What do we do in this case?  */ 
  }

  chSysUnlockFromISR();
}

#include <ll/ll_button.h>

static uint32_t ll_button_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

static uint32_t ll_button_data_available(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

static uint32_t ll_button_data_writeable(struct ll_driver_s *this) {
  (void) this;
  return 0;
}

static uint32_t ll_button_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_button_driver_t *b = (ll_button_driver_t*)this->driver_info;

  uint32_t r = 0;
  
  if (data_size == 4) {
    data[0] = b->internal.state;
    data[1] = b->internal.state >> 8;
    data[2] = b->internal.state >> 16;
    data[3] = b->internal.state >> 24;
    r = 4;
  }
  
  return r;
}

static uint32_t ll_button_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

bool ll_button_init(ll_driver_t* lld, ll_button_driver_t *bdrv) {

  lld->driver_info = bdrv;
 
  lld->is_synchronous = false;
  lld->ll_control_fun = ll_button_control;
  lld->ll_read_fun = ll_button_read;
  lld->ll_write_fun = ll_button_write;
  lld->ll_data_readable_fun = ll_button_data_available;
  lld->ll_data_writeable_fun = ll_button_data_writeable;
  return true;
}
