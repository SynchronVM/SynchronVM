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

#ifndef LTR_303ALS_H_
#define LTR_303ALS_H_

#define I2C_ADDR        0x29

#define ALS_CONTROL_REG 0x80
#define ALS_RESET       0x1
#define ALS_ACTIVE      0x2

#define ALS_DATA_CH_0_LOW   0x8A
#define ALS_DATA_CH_0_HIGH  0x8B
#define ALS_DATA_CH_1_LOW   0x88
#define ALS_DATA_CH_1_HIGH  0x89

extern bool als_init(void);
extern bool als_read_data(uint16_t *ch0, uint16_t *ch1);
extern bool als_status(void);
extern bool als_set_gain(uint8_t gain);

#endif
