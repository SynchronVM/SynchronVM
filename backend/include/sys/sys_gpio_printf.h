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

#ifndef SYS_GPIO_PRINTF_
#define SYS_GPIO_PRINTF_

#include <vm-conf.h>

#if VMC_ENABLE_GPIO_PRINTF == 1
  
#include <platform_gpio_printf.h>

#define DBG_GPIO_INIT(X) PLATFORM_DBG_GPIO_INIT(X) 

#define DBG_GPIO_SET(X) PLATFORM_DBG_GPIO_SET(X) 
#define DBG_GPIO_CLR(X) PLATFORM_DBG_GPIO_CLR(X) 
#define DBG_GPIO_GET(X) PLATFORM_DBG_GPIO_GET(X)

#else

#define DBG_GPIO_INIT(X)

#define DBG_GPIO_SET(X)
#define DBG_GPIO_CLR(X)
#define DBG_GPIO_GET(X)

#endif 

#endif
