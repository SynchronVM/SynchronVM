/****************************/
/* LTR-303ALS               */
  
#include <drivers/i2c.h>
#include <ltr_303als.h>

int init_als(const struct device *dev, uint8_t gain) {

  return i2c_reg_write_byte (dev, I2C_ADDR, ALS_CONTROL_REG, gain);
 
}


int read_data_als(const struct device *dev, uint16_t *ch0, uint16_t *ch1) { 

  uint8_t ch_0_low;
  uint8_t ch_0_high;
  uint8_t ch_1_low;
  uint8_t ch_1_high;

  
  if (i2c_reg_read_byte (dev,I2C_ADDR, ALS_DATA_CH_1_LOW, &ch_1_low)) {
    return -1;
  }
  if (i2c_reg_read_byte (dev,I2C_ADDR, ALS_DATA_CH_1_HIGH, &ch_1_high)) {
    return -1;
  }

  if (i2c_reg_read_byte (dev,I2C_ADDR, ALS_DATA_CH_0_LOW, &ch_0_low)) {
    return -1;
  }
  if (i2c_reg_read_byte (dev,I2C_ADDR, ALS_DATA_CH_0_HIGH, &ch_0_high)) {
    return -1;
  }
  /* Reading ALS_DATA_CH_0_HIGH triggers a new ADC conversion so 
     it should be read last */ 

  uint16_t c0 = ch_0_high << 8 | ch_0_low;
  uint16_t c1 = ch_1_high << 8 | ch_1_low;

  *ch0 = c0;
  *ch1 = c1;
  
  return 0;
}
