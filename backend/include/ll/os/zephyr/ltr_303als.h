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

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define I2C_ADDR        0x29

#define ALS_CONTROL_REG         0x80
#define ALS_MEAS_RATE_REG       0x85
#define ALS_PART_ID_REG         0x86
#define ALS_MANUFACT_ID_REG     0x87
#define ALS_DATA_CH_1_LOW_REG   0x88
#define ALS_DATA_CH_1_HIGH_REG  0x89
#define ALS_DATA_CH_0_LOW_REG   0x8A
#define ALS_DATA_CH_0_HIGH_REG  0x8B
#define ALS_STATUS_REG          0x8C
#define ALS_INTERRUPT_REG       0x8F
#define ALS_THRES_UP_0_REG      0x97
#define ALS_THRES_UP_1_REG      0x98
#define ALS_THRES_LOW_0_REG     0x99
#define ALS_THRES_LOW_1_REG     0x9A

#define ALS_RESET       0x1
#define ALS_ACTIVE      0x2

#define ALS_STATUS_DATA_VALID_MASK 0b10000000
#define ALS_STATUS_GAIN_MASK       0b01110000
#define ALS_STATUS_INTERRUPT_MASK  0b00001000
#define ALS_STATUS_DATA_NEW_MASK   0b00000100

#define ALS_GAIN_1X  0x0
#define ALS_GAIN_2X  0x1
#define ALS_GAIN_4X  0x2
#define ALS_GAIN_8X  0x3
#define ALS_GAIN_48X 0x6
#define ALS_GAIN_96X 0x7

/* Adjusted these upwards from the datasheet until it seemed to work reliably */ 
#define ALS_STARTUP_TIME_MS 200 /*100*/
#define ALS_WAKEUP_TIME_MS  100 /*10*/
#define ALS_STANDBY_TIME_MS 100 /*10*/

/* 
   
** ALS_CONTROL_REG 0x80

  7 Reserved
  6 Reserved
  5 Reserved
  4 Gain
  3 Gain
  2 Gain
  1 Reset (1 when startup process ongoing 0 after)
  0 Standby (0 standby : 1 active) 
  
 */ 

extern bool als_init(void);
extern bool als_standby(void);
extern bool als_activate(uint8_t gain);

extern bool als_read_data(uint16_t *ch0, uint16_t *ch1);
extern bool als_read_data_bytes(uint8_t *ch0_low, uint8_t *ch0_high,
				uint8_t *ch1_low, uint8_t *ch1_high);
extern bool als_read_data_sequential(uint8_t *data, uint32_t data_size);
extern bool als_status(uint8_t *als_status);

#endif
