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


/****************************/
/* LTR-303ALS               */

#include <drivers/i2c.h>
#include <ltr_303als.h>

const struct device *ltr_303als_dev;

bool als_init(void) {

  ltr_303als_dev = device_get_binding("I2C_0");
  return (bool)ltr_303als_dev;
}

bool als_status(void) {
  return (bool)ltr_303als_dev;
}

bool als_set_gain(uint8_t gain) {

  /* i2c_reg_write_byte return 0 on success and a negative number of failure */
  if (!i2c_reg_write_byte (ltr_303als_dev, I2C_ADDR, ALS_CONTROL_REG, gain)) {
    return true;
  }
  return false;
}


bool als_read_data(uint16_t *ch0, uint16_t *ch1) {

  uint8_t ch_0_low;
  uint8_t ch_0_high;
  uint8_t ch_1_low;
  uint8_t ch_1_high;


  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_1_LOW, &ch_1_low)) {
    return false;
  }
  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_1_HIGH, &ch_1_high)) {
    return false;
  }

  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_0_LOW, &ch_0_low)) {
    return false;
  }
  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_0_HIGH, &ch_0_high)) {
    return false;
  }
  /* Reading ALS_DATA_CH_0_HIGH triggers a new ADC conversion so
     it should be read last */

  uint16_t c0 = ch_0_high << 8 | ch_0_low;
  uint16_t c1 = ch_1_high << 8 | ch_1_low;

  *ch0 = c0;
  *ch1 = c1;

  return true;
}
