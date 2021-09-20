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

#ifndef CHIBIOS_PWM_H_
#define CHIBIOS_PWM_H_

#include "hal.h"
#include "hal_pwm.h"
#include "hal_pal.h"

/* Experiment - proof of concept */
/* TODO: Improve */


typedef struct {
  PWMConfig pwmc;
  PWMDriver *pwm;
} pwm_driver_group_internal_t;

#define PWM_DRIVER_GROUP_INTERNAL pwm_driver_group_internal_t internal

typedef struct {
  uint32_t id;
  uint32_t channel;
  pwm_driver_group_internal_t *group;
} pwm_driver_internal_t;

#define PWM_DRIVER_INTERNAL pwm_driver_internal_t internal


#define PWM_DRIVER_GROUP_INTERNAL_INIT(XgroupX, XgidX) \
  XgroupX.internal.pwmc.frequency = PWM##XgidX##_FREQ;\
  XgroupX.internal.pwmc.period    = PWM##XgidX##_PERIOD;\
  XgroupX.internal.pwmc.callback  = NULL;\
  XgroupX.internal.pwmc.channels[0].mode = 0;\
  XgroupX.internal.pwmc.channels[0].callback = NULL;\
  XgroupX.internal.pwmc.channels[1].mode = 0;\
  XgroupX.internal.pwmc.channels[1].callback = NULL;	\
  XgroupX.internal.pwmc.channels[2].mode = 0;\
  XgroupX.internal.pwmc.channels[2].callback = NULL;	\
  XgroupX.internal.pwmc.channels[3].mode = 0;	\
  XgroupX.internal.pwmc.channels[3].callback = NULL;	\
  XgroupX.internal.pwm = &PWM##XgidX##_DRIVER;


#define PWM_DRIVER_INTERNAL_INIT(XpdrvX, XgroupX, XgidX, XchidX, Xdrv_idX) \
  XpdrvX.internal.channel = XchidX;\
  XpdrvX.internal.id = Xdrv_idX;\
  XpdrvX.internal.group = &XgroupX;\
  XgroupX.internal.pwmc.channels[XchidX].mode = PWM_OUTPUT_ACTIVE_HIGH;\
  palSetPadMode( PWM##XgidX##_CH##XchidX##_GPIO, PWM##XgidX##_CH##XchidX##_PIN, PWM##XgidX##_CH##XchidX##_MODE);
  
  

#define PWM_DRIVER_GROUP_INTERNAL_START(XgroupX)		\
  pwmStart(XgroupX.internal.pwm, &XgroupX.internal.pwmc); 


#endif
