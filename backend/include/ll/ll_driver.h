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

#define LL_DRIVER_CONTROL_FAILURE 0x0
#define LL_DRIVER_CONTROL_SUCCESS 0x1

extern void (*ll_driver_sleep_ms_fun)(uint32_t ms);
extern uint64_t (*ll_driver_timestamp_fun)();


/* Initialize driver subsystem. 
   sleep_ms  : pointer to sleep function or NULL 
   timestamp : pointer to timestamp function of NULL 
*/ 
extern bool ll_driver_init(void (*sleep_ms)(uint32_t),
			   uint64_t (*timestamp)());

/* Sleep and timestamp functions for use in driver implementation */
extern void ll_driver_sleep_ms(uint32_t ms);
extern uint64_t ll_driver_timestamp();

/* Potentially register a hook with a driver.
   If the driver is interrupt based it may be used
   to kickstart some other part of the rts? perhaps...
   Maybe this should be an argument to the init
   routine of the driver in question?
*/
//typedef bool (*ll_driver_hook_fun)(uint32_t *arg);

/* Driver interface */
typedef struct ll_driver_s{
  void *driver_info;
  uint32_t (*ll_read_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  uint32_t (*ll_write_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  uint32_t (*ll_control_fun)(struct ll_driver_s *this, uint8_t *, uint32_t);
  bool (*ll_data_available_fun)(struct ll_driver_s *this);
} ll_driver_t;

inline int ll_read(ll_driver_t *drv, uint8_t *data, uint32_t data_size) {
  return drv->ll_read_fun((struct ll_driver_s*)drv, data, data_size);
}

inline int ll_write(ll_driver_t *drv, uint8_t *data, uint32_t data_size) {
  return drv->ll_write_fun((struct ll_driver_s*)drv, data, data_size);
}

inline bool ll_data_available(ll_driver_t *drv) { /* bytes available */
  return drv->ll_data_available_fun((struct ll_driver_s*)drv);
}

// inline uint32_t ll_control(struct ll_driver_s *this, uint8_t *, uint32_t);

/* Message format, driver -> rts */

typedef struct ll_driver_msg_s{
  uint8_t  driver_id;     // Index into an array of drivers maintained by "low-level" 
  uint8_t  message;       // Driver specific message
  uint64_t timestamp;  
} ll_driver_msg_t;


#endif
