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

/* Note that VALID data has VALID bit set to 0 
   So really it is an "invalid"-bit */

extern bool als_status(uint8_t *als_status){

  bool r = false;
  if (!i2c_reg_read_byte(ltr_303als_dev, I2C_ADDR, ALS_STATUS_REG, als_status)) {
    r = true;
  } else {
    r = false;
  }
  return r;  
}

bool als_activate(uint8_t gain) {
  bool r = false;

  uint8_t ctrl_reg = 0x1 | (gain << 2);
  
  if (!i2c_reg_write_byte(ltr_303als_dev, I2C_ADDR, ALS_CONTROL_REG, ctrl_reg)) {
    r = true;
  } else {
    r = false;
  }
  return r;
}

/* Set the sensor in standby mode. 
   use als_activate to turn back on. 

   Standby mode 5uA. 
   The pull-ups on the data-lines will always consume some extra current. 
*/ 
bool als_standby(void) {
  bool r = false;

  uint8_t ctrl_reg = 0x0;
  
  if (!i2c_reg_write_byte(ltr_303als_dev, I2C_ADDR, ALS_CONTROL_REG, ctrl_reg)) {
    r = true;
  } else {
    r = false;
  }
  return r;
}

/*
  ALS data registers should be read from the lowest address to the 
  highest. 0x88, 0x89, 0x8A, 0x8B (so channel 1 before channel 0 -- odd naming..).
 
  When starting to read data, all 4 registers are locked until the read of 
  0x8B has finished. So even if the sensor finishes off new samples while 
  we are reading data, we will get consistent values. 
 */



bool als_read_data_sequential(uint8_t *data, uint32_t data_size) {
  bool r = true;

  static int offset = 0; 

  for (int i = 0; i < data_size; i ++)  {
    r = r && i2c_reg_read_byte(ltr_303als_dev,I2C_ADDR, (ALS_DATA_CH_1_LOW_REG + offset), &data[i]);
    offset = (offset + 1) % 4;

    if (!r) break;
  }
  return r;
}

bool als_read_data_bytes(uint8_t *ch0_low, uint8_t *ch0_high,
			 uint8_t *ch1_low, uint8_t *ch1_high) {

  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_1_HIGH_REG, ch1_low)) {
    return false;
  }
  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_1_HIGH_REG, ch1_high)) {
    return false;
  }

  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_0_LOW_REG, ch0_low)) {
    return false;
  }
  if (i2c_reg_read_byte (ltr_303als_dev,I2C_ADDR, ALS_DATA_CH_0_HIGH_REG, ch0_high)) {
    return false;
  }
  /* Reading ALS_DATA_CH_0_HIGH triggers a new ADC conversion so
     it should be read last */
  return true;
}

bool als_read_data(uint16_t *ch0, uint16_t *ch1) {

  uint8_t ch0_low;
  uint8_t ch0_high;
  uint8_t ch1_low;
  uint8_t ch1_high;

  bool r; 
  
  r = als_read_data_bytes(&ch0_low,&ch0_high,&ch1_low,&ch1_high);

  if (r) { 
    uint16_t c0 = ch0_high << 8 | ch0_low;
    uint16_t c1 = ch1_high << 8 | ch1_low;
    
    *ch0 = c0;
    *ch1 = c1;
  }
  return r;
}


/* LL interface implementation */


#include <ll/ll_als.h>


static uint32_t ll_als_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

static uint32_t ll_als_data_available(struct ll_driver_s *this) {
  return 4; // not really sure what to do here. the driver should be redesigned
}
static uint32_t ll_als_data_writeable(struct ll_driver_s *this) {
  return 0;
}

static uint32_t ll_als_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {

  bool r = als_read_data_sequential(data, data_size);

  
  return r ? data_size : 0 ; /* makes slightly more sense, But the error should be reported somehow. */ 
}

/* als does not accept any input */
static uint32_t ll_als_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0; 
}

bool ll_als_init(ll_driver_t* lld, uint8_t gain) {

  bool r = als_init();

  if (r) ll_driver_sleep_ms(ALS_STARTUP_TIME_MS);

  r = als_standby();
  
  if (r) ll_driver_sleep_ms(ALS_STANDBY_TIME_MS);
  
  r = als_activate(gain);

  if (r) {
    lld->driver_info = NULL;
    lld->is_synchronous = true;
    lld->ll_control_fun = ll_als_control;
    lld->ll_read_fun = ll_als_read;
    lld->ll_write_fun = ll_als_write;
    lld->ll_data_readable_fun = ll_als_data_available;
    lld->ll_data_writeable_fun = ll_als_data_writeable;
  }

  if(r) ll_driver_sleep_ms(ALS_WAKEUP_TIME_MS);
  
  return r;

}
