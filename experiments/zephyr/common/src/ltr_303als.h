
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

extern int init_als(const struct device *dev, uint8_t gain);
extern int read_data_als(const struct device *dev, uint16_t *ch0, uint16_t *ch1);

#endif
