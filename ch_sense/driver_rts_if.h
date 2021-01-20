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

#ifndef _DRIVER_RTS_IF_
#define _DRIVER_RTS_IF_

#include "typedefs.h"
#include <stdbool.h>

#define DRIVER_FLAGS_NONE    0x00
#define DRIVER_RDY_RECV_MASK 0x01
#define DRIVER_RDY_SEND_MASK 0x02
#define DRIVER_OK            0x80 

typedef struct driver_rts_if_s{
  volatile uint8_t flags; /* rdy_recv, rdy_send, maybe more? */

  cam_value_t (*recv)(struct driver_rts_if_s *this);
  bool (*send)(struct driver_rts_if_s *this, cam_value_t);

} driver_rts_if_t;


inline void driver_clear_rdy_recv_bit(driver_rts_if_t* drv) {
  drv->flags = drv->flags & ~(DRIVER_RDY_RECV_MASK);
}

inline void driver_clear_rdy_send_bit(driver_rts_if_t* drv) {
  drv->flags = drv->flags & ~(DRIVER_RDY_RECV_MASK);
}

inline void driver_set_rdy_recv_bit(driver_rts_if_t* drv) {
  drv->flags &= DRIVER_RDY_RECV_MASK;
}

inline void driver_set_rdy_send_bit(driver_rts_if_t* drv) {
  drv->flags &= DRIVER_RDY_SEND_MASK;
}

inline bool driver_rdy_recv(driver_rts_if_t* drv) {
  return (drv->flags & DRIVER_RDY_RECV_MASK) ? true : false;
}

inline bool driver_rdy_send(driver_rts_if_t* drv) {
  return (drv->flags & DRIVER_RDY_SEND_MASK) ? true : false; // feels a bit dumb.... 
}

inline void driver_send(driver_rts_if_t* drv, cam_value_t val) {
  drv->send(drv, val);
}

inline cam_value_t driver_recv(driver_rts_if_t* drv) {
  return drv->recv(drv);
}
  

#endif
