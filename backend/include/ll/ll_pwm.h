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

#ifndef LL_PWM_H_
#define LL_PWM_H_

#include <pwm.h>
#include <ll/ll_driver.h>

typedef struct {
  PWM_DRIVER_INTERNAL;
} ll_pwm_driver_t;

typedef struct {
  PWM_DRIVER_GROUP_INTERNAL;
} ll_pwm_driver_group_t; 


#define LL_PWM_DRIVER_GROUP_INIT(XgroupX, XgidX) \
  PWM_DRIVER_GROUP_INTERNAL_INIT(XgroupX, XgidX);

#define LL_PWM_DRIVER_INIT(XpdrvX, XgroupX, XgidX,  XpidX, Xdrv_idX)	\
  PWM_DRIVER_INTERNAL_INIT(XpdrvX, XgroupX, XgidX, XpidX, Xdrv_idX);

#define LL_PWM_DRIVER_GROUP_START(XgroupX) \
  PWM_DRIVER_GROUP_INTERNAL_START(XgroupX);

extern bool ll_pwm_init(ll_driver_t *lld, ll_pwm_driver_t *pdrv);

#endif
