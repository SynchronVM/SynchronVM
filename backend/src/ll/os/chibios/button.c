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
#include <ll/ll_sys_time.h>

#include <hal_pal.h>
uint32_t button_num(void){
  return gpio_num_buttons();
}

static void button_cb(void *arg) {

  chSysLockFromISR();
  
  button_driver_t *button = (button_driver_t *)arg;

  bool state = palReadPad(button->port, button->pad);

  button->state = state;
  
  ll_driver_msg_t msg; 
  msg.driver_id = button->id; 
  msg.timestamp.high_word = 0; //ll_driver_timestamp();
  msg.timestamp.low_word = 0;
  msg.data = state;  // 1 or 0

  if (button->interop->send_message(button->interop, msg) == -1) {
    /* Message was not send due to queue being full. 
       What do we do in this case?  */ 
  }

  chSysUnlockFromISR();
}

button_driver_t button_drivers[10]; /*hmm I dont like this much */


button_driver_t *button_init(uint32_t drv_id, void *backend_custom, uint32_t identifier) {
  (void) drv_id; /* hmm */ 
  button_driver_t *r = NULL;
  
  if (identifier < button_num()) {
    palSetPadMode(buttons[identifier].port,
		  buttons[identifier].pad,
		  buttons[identifier].mode);

    button_drivers[identifier].port = buttons[identifier].port;
    button_drivers[identifier].pad = buttons[identifier].pad;
    button_drivers[identifier].id = identifier;
    button_drivers[identifier].state = false;
    button_drivers[identifier].interop = (chibios_interop_t*)backend_custom;
    palEnablePadEvent(buttons[identifier].port, buttons[identifier].pad, buttons[identifier].event_mode);
    palSetPadCallback(buttons[identifier].port, buttons[identifier].pad, button_cb, &button_drivers[identifier]);
    
    r = &button_drivers[identifier];
  }
  return r;

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
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

bool ll_button_init(ll_driver_t* lld, uint32_t drv_id, void* backend_custom,  uint32_t button_id) {

  button_driver_t *button_driver = button_init(drv_id, backend_custom, button_id);

  bool r = false;
  
  if (button_driver) {
    r = true; 
    lld->driver_info = (void*) button_driver;
    lld->is_synchronous = false;
    lld->ll_control_fun = ll_button_control;
    lld->ll_read_fun = ll_button_read;
    lld->ll_write_fun = ll_button_write;
    lld->ll_data_readable_fun = ll_button_data_available;
    lld->ll_data_writeable_fun = ll_button_data_writeable;
  }
  return r;
}
