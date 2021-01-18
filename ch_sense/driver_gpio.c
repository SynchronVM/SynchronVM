/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "driver_rts_if.h"
#include "driver_gpio.h"
#include "typedefs.h"      

/* GPIO
   
   GPIO is a bit of a funny case because as soon as the 
   purpose of a pin has been decided for a specific io pin 
   in a finished process, it is no longer "General purpose". 
   
   GPIO is a kind of temporary situation for experimentation  
   and development. 

   To support "GPIO" fully one would want a lot of configuration
   capabilities exposed to tinker with. Input/Output, Pull up/down or
   float and so on. 

 */ 

#define DRIVER_GPIO_COMMAND_MASK  0xFFFF0000
#define DRIVER_GPIO_PIN_MASK      0x0000FFFF


#define DRIVER_GPIO_GET(X) ((X) & DRIVER_GPIO_PIN_MASK) 

/* Sketch built on ChibiOS */
#include <ch.h>
#include <hal.h>
#include <hal_pal.h>

/* Board dependent code */

typedef struct {
  stm32_gpio_t *port;
  uint16_t pad;
} driver_io_pad_t;

driver_io_pad_t pads[] =
  {{GPIOA, 0},
   {GPIOA, 1},
   {GPIOA, 2},
   {GPIOA, 3},
   {GPIOA, 4},
   {GPIOA, 5},
   {GPIOA, 6},
   {GPIOA, 7}};
  

cam_value_t driver_send_buffer;




static cam_value_t recv(driver_rts_if_t *this) { // used to send data from driver

  driver_clear_rdy_send_bit(this);
  return driver_send_buffer;
}


static bool send(driver_rts_if_t *this, cam_value_t value) {  // used to send data to driver 

  uint16_t cmd = (uint16_t) (value.value >> 16);
  uint16_t pad = (uint16_t) value.value;

  switch (cmd) {
  case DRIVER_GPIO_COMMAND_READ:
    driver_send_buffer.value = palReadPad(pads[pad].port,pads[pad].pad);
    driver_set_rdy_send_bit(this);
    break;
  case DRIVER_GPIO_COMMAND_SET:
    palSetPad(pads[pad].port, pads[pad].pad);
    break;
  case DRIVER_GPIO_COMMAND_CLR:
    palClearPad(pads[pad].port, pads[pad].pad);
    break;
  default:
    break;
  }
  return true;
}

bool init_gpio_driver(driver_rts_if_t *drv) {

  for (int i = 0; i < 8; i ++) {

    palSetPadMode(pads[i].port, pads[i].pad,
		 PAL_MODE_OUTPUT_PUSHPULL |
		 PAL_STM32_OSPEED_HIGHEST);

    palClearPad(pads[i].port, pads[i].pad);
  }


  
  drv->flags = DRIVER_OK;
  drv->recv = recv;
  drv->send = send;

  driver_set_rdy_recv_bit(drv); // driver is ready to recv 
  
  return true;
}
