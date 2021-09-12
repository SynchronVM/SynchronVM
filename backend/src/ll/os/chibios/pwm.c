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

#include <pwm.h>
#include <ll/ll_pwm.h>
#include <string.h>

/* HACK! */
#include "usbcfg.h"


static uint32_t ll_pwm_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

static uint32_t ll_pwm_data_available(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

static uint32_t ll_pwm_data_writeable(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

static uint32_t ll_pwm_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {

  memset(data, 0, data_size);

  return data_size;
}

static uint32_t ll_pwm_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_pwm_driver_t *pwm_driver = (ll_pwm_driver_t*)this->driver_info;

  if (data_size >= 2) {

    uint32_t duty = data[1];
    duty = duty << 8;
    duty |= data[0];

    PWMDriver *pwm = pwm_driver->internal.group->pwm;
    
    
    pwmEnableChannel(pwm,
		     pwm_driver->internal.channel,
		     PWM_PERCENTAGE_TO_WIDTH(pwm, duty));
    /* pwmEnableChannel(&PWMD3, 0 , PWM_PERCENTAGE_TO_WIDTH(&PWMD3, duty)); */
    /* pwmEnableChannel(&PWMD3, 1 , PWM_PERCENTAGE_TO_WIDTH(&PWMD3, duty)); */
    /* pwmEnableChannel(&PWMD3, 2 , PWM_PERCENTAGE_TO_WIDTH(&PWMD3, duty)); */
    /* pwmEnableChannel(&PWMD3, 3 , PWM_PERCENTAGE_TO_WIDTH(&PWMD3, duty)); */
  }
  return data_size;
}


bool ll_pwm_init(ll_driver_t* lld, ll_pwm_driver_t *pdrv){

  lld->driver_info = pdrv;
  lld->is_synchronous = true;
  lld->ll_control_fun = ll_pwm_control;
  lld->ll_read_fun = ll_pwm_read;
  lld->ll_write_fun = ll_pwm_write;
  lld->ll_data_readable_fun = ll_pwm_data_available;
  lld->ll_data_writeable_fun = ll_pwm_data_writeable;

  return true;
}
