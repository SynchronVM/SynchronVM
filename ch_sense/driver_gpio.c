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


static cam_value_t recv(void) { // used to send data from driver

}


static send(cam_value_t) {  // used to send data to driver 

  
  
  
}

bool init_gpio_driver(driver_rts_if_t *drv) {

  drv.flags = DRIVER_OK;
  drv.recv = recv;
  drv.send = send;

  return true;
}
