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

#ifndef LL_DRIVER_H_
#define LL_DRIVER_H_

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <svmdefs.h>

#include <sys/sys_time.h>

/* Thoughts: We have not used the control interface for anything so far. Maybe drop it. */

#define LL_DRIVER_CONTROL_FAILURE 0x0
#define LL_DRIVER_CONTROL_SUCCESS 0x1

extern bool ll_driver_init(void);

/* Driver interface */
typedef struct ll_driver_s{
  void *driver_info;
  bool is_synchronous;
  uint32_t (*ll_read_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  uint32_t (*ll_write_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  uint32_t (*ll_control_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  uint32_t (*ll_data_readable_fun)(struct ll_driver_s *this);
  uint32_t (*ll_data_writeable_fun)(struct ll_driver_s *this);

  UUID channel_id;
} ll_driver_t;

extern uint32_t ll_read(ll_driver_t *drv, uint8_t *data, uint32_t data_size);

extern uint32_t ll_write(ll_driver_t *drv, uint8_t *data, uint32_t data_size);

extern uint32_t ll_data_readable(ll_driver_t *drv);

extern uint32_t ll_data_writeable(ll_driver_t *drv);

#endif
