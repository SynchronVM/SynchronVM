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

#ifndef CHIBIOS_DAC_H_
#define CHIBIOS_DAC_H_

#include <stdbool.h>
#include <stdint.h>

#include <gpio.h>
#include <svm_chibios_conf.h>

typedef struct {
  stm32_gpio_t *port;
  uint16_t pad;
  uint32_t id;
  uint32_t state;
  DACDriver *dacd;
  DACConfig dacc;
} dac_driver_internal_t;

/* #define CONC(a,b) a##_##b */

/* #define IF(c, t, e) CONC(IF, c)(t, e) */
/* #define IF_1(t, e) e */
/* #define IF_2(t, e) t */
/* #define THE_DAC(x)  IF(x, DACD1, DACD1) */

#define DAC_DRIVER_INTERNAL dac_driver_internal_t internal

static const DACConfig dac_config = {
  .init         = 2047u, 
  .datamode     = DAC_DHRM_12BIT_RIGHT,
  .cr           = 0
};

/*
  palSetPadMode(DAC##XdidX##_GPIO,		\
                DAC##XdidX##_PIN,\
                DAC##XdidX##_MODE);\
  
  XddrvX.internal.port = DAC##XdidX##_GPIO;\
  XddrvX.internal.pad = DAC##XdidX##_PIN;\
  XddrvX.internal.id  = Xdrv_idX;\
  XddrvX.internal.state = 2047;\
  \
*/

#define DAC_DRIVER_INTERNAL_INIT(XddrvX, XdidX, Xdrv_idX)\
  palSetPadMode(DAC##XdidX##_GPIO,			 \
		DAC##XdidX##_PIN,			 \
		DAC##XdidX##_MODE);			 \
  XddrvX.internal.dacc.init = 2047u;\
  XddrvX.internal.dacc.datamode = DAC_DHRM_12BIT_RIGHT;\
  XddrvX.internal.dacc.cr = 0;\
  XddrvX.internal.dacd = &DACD1;\
  XddrvX.internal.id  = Xdrv_idX;\
  dacStart(XddrvX.internal.dacd, &XddrvX.internal.dacc);

#endif
