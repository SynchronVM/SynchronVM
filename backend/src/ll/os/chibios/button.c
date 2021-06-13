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

#include <hal_pal.h>
uint32_t button_num(void){
  return gpio_num_buttons();
}


static void button_cb(void *arg) {

  chSysLockFromISR();

  button_driver_t *button = (chibios_interop_t*)arg;

  bool state = palReadPad(button->port, button->pad);

  button->state = state;
  
  ll_driver_msg_t msg; 
  msg.driver_id = button->id; 
  msg.timestamp = 0; //ll_driver_timestamp();
  msg.data = state;  // 1 or 0

  if (button->interop->send_message(button->interop, msg) == -1) {
    /* Message was not send due to queue being full. 
       What do we do in this case?  */ 
  }

  chSysUnlockFromISR();
}

button_driver_t button_drivers[10]; /*hmm I dont like this much */


button_driver_t *button_init(uint32_t drv_id, void *backend_custom, uint32_t identifier) {
  button_driver_t *r = NULL;
  
  if (identifier < button_num()) {
    palSetPadMode(buttons[identifier].port,
		  buttons[identifier].pad,
		  buttons[identifier].mode);

    button_drivers[identifier].port = leds[identifier].port;
    button_drivers[identifier].pad = leds[identifier].pad;
    button_drivers[identifier].id = identifier;
    button_drivers[identifier].state = false;
    button_drivers[identifier].interop = (chibios_interop_t*)backend_custom;
    palEnablePadEvent(buttons[identifier].port, buttons[identifier].pad, PAL_EVENT_MODE_BOTH_EDGES);
    palSetPadCallback(buttons[identifier].port, buttons[identifier].pad, button_cb, &button_drivers[identifier]);
    
    r = &button_drivers[identifier];
  }
  return r;

}
