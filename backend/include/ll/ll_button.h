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

#ifndef LL_BUTTON_H_
#define LL_BUTTON_H_

#include <stdint.h>
#include <stdbool.h>

#include <button.h>
#include <ll/ll_driver.h>
#include <CONFIG_DEFINES.h>

typedef struct {
  BUTTON_DRIVER_INTERNAL;
} ll_button_driver_t;

#define LL_BUTTON_DRIVER_INIT(XbdrvX, XbidX, Xdrv_idX, XcustomX) \
  BUTTON_DRIVER_INTERNAL_INIT(XbdrvX, XbidX, Xdrv_idX, XcustomX)

#define LL_BUTTON_DRIVER_INTERRUPT_MODE(XbdrvX, XbidX, XMX) \
  BUTTON_DRIVER_INTERNAL_INTERRUPT_MODE(XbdrvX, XbidX, XMX)

extern bool ll_button_init(ll_driver_t* lld, ll_button_driver_t *bdrv);

  
#endif

