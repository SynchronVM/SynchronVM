/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2022 Joel Svensson, Abhiroop Sarkar 				  */
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


#include <midi_uart.h>
#include <ch.h>
#include <hal.h>
#include <hal_pal.h>

SerialConfig midi_uart_serial_def_conf = {
  31250,
  0,
  USART_CR2_STOP1_BITS,
  0
};

#include <ll/ll_midi.h>

static uint32_t ll_midi_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

static uint32_t ll_midi_data_available(struct ll_driver_s *this) {
  (void) this;
  return 0; /* Disabled: should be implement as interrupt */
}

static uint32_t ll_midi_data_writeable(struct ll_driver_s *this) {
  (void) this;
  return 0;
}

static uint32_t ll_midi_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_midi_driver_t *m = (ll_midi_driver_t*)this->driver_info;

  (void) m;
  /* uint32_t r = 0; */
  
  /* if (data_size == 4) { */
  /*   data[0] = b->internal.state; */
  /*   data[1] = b->internal.state >> 8; */
  /*   data[2] = b->internal.state >> 16; */
  /*   data[3] = b->internal.state >> 24; */
  /*   r = 4; */
  /* } */
  
  return 0;
}

static uint32_t ll_midi_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_midi_driver_t *m = (ll_midi_driver_t*)this->driver_info;

  int r = 0;
  /* Locking this to only work with midi 3 byte commants. 
     encoded into a 4 byte word */
  if (data_size == 4) {
  sdWriteTimeout(m->internal.serial_driver, data, 3, 500);
  r = 4;
  }
  return r;
}

bool ll_midi_init(ll_driver_t* lld, ll_midi_driver_t *mdrv) {

  lld->driver_info = mdrv;
 
  lld->is_synchronous = true;
  lld->ll_control_fun = ll_midi_control;
  lld->ll_read_fun = ll_midi_read;
  lld->ll_write_fun = ll_midi_write;
  lld->ll_data_readable_fun = ll_midi_data_available;
  lld->ll_data_writeable_fun = ll_midi_data_writeable;
  return true;
}
