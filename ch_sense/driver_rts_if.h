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

#ifndef _RTS_DRIVER_IF_
#define _RTS_DRIVER_IF_

#include "typedefs.h"
#include <stdbool.h>

#define RDY_RECV_MASK 0x01
#define RDY_SEND_MASK 0x02

typedef struct {
  volatile uint8_t flags; /* rdy_recv, rdy_send, maybe more? */

  cam_value_t (*recv)();
  bool (*send)(cam_value_t);

} driver_rts_if_t;

#endif
