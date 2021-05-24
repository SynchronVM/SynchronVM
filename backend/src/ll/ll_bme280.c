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

#include <ll/ll_bme280.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

static uint32_t ll_bme280_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0;
}

static uint32_t ll_bme280_data_available(struct ll_driver_s *this) {
  return 12;
}

static uint32_t ll_bme280_data_writeable(struct ll_driver_s *this) {
  return 0;
}


/* Maybe this should be split up into 3 different read channels? 
   Will have to think about that in relation to how IOChannels will look 
   
   I have this similar feeling with some of the other drivers.... look into it. 
*/

static uint32_t ll_bme280_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  // float_val = val1 + val2 * 10^(-6)
  int32_t t = 0, td = 0;
  int32_t p = 0, pd = 0;
  int32_t h = 0, hd = 0;

  uint32_t data_read = 0;
    
  bool r = bme_sample();

  r = r && bme_get_temperature(&t, &td);
  r = r && bme_get_pressure(&p, &pd);
  r = r && bme_get_humidity(&h, &hd);

  if (r) { 
    float temp = (float)t + ((float)td * 0.0000001);
    float pres = (float)p + ((float)pd * 0.0000001);
    float humi = (float)h + ((float)hd * 0.0000001);
    
    if (data_size >= 4) {
      data[0] = ((uint8_t*)&temp)[0];
      data[1] = ((uint8_t*)&temp)[1];
      data[2] = ((uint8_t*)&temp)[2];
      data[3] = ((uint8_t*)&temp)[3];
      data_read = 4;
    }

    if (data_size >= 8) {
      data[4] = ((uint8_t*)&pres)[0];
      data[5] = ((uint8_t*)&pres)[1];
      data[6] = ((uint8_t*)&pres)[2];
      data[7] = ((uint8_t*)&pres)[3];
      data_read = 8;
    }

    if (data_size >= 12) {
      data[8]  = ((uint8_t*)&humi)[0];
      data[9]  = ((uint8_t*)&humi)[1];
      data[10] = ((uint8_t*)&humi)[2];
      data[11] = ((uint8_t*)&humi)[3];
      data_read = 12;
    }
  }
  
  return data_read;
}

/* bme280 does not accept any input */
static uint32_t ll_bme280_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  return 0; 
}

bool ll_bme280_init(ll_driver_t* lld) {

  bool r = bme_init();

  if (r) {
    lld->driver_info = NULL;
    lld->ll_control_fun = ll_bme280_control;
    lld->ll_read_fun = ll_bme280_read;
    lld->ll_write_fun = ll_bme280_write;
    lld->ll_data_readable_fun = ll_bme280_data_available;
    lld->ll_data_writeable_fun = ll_bme280_data_writeable;
  }
  
  return r;

}
