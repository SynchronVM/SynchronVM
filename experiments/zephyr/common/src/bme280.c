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

#include <drivers/sensor.h>
#include <bme280.h>

const struct device *bme280_dev;


bool bme_init(void) {
  bme280_dev = device_get_binding(BME280_LABEL);

  return (bool)bme280_dev;
}



/* Trigger a sampling of the sensor */
bool bme_sample(void) {
  /* returns value != 0 on failure */
  if (sensor_sample_fetch(bme280_dev)) {
    return false;
  }
  return true;
}

/* sensor values are stored as two integers (val1, val2)
   a float representation can be computed as:
   float_val = val1 + val2 * 10^(-6)
*/

static bool bme_sensor_get(enum sensor_channel chan, int32_t *i, int32_t *d) {

  struct sensor_value t;
  int r = sensor_channel_get(bme280_dev, chan, &t);

  if (r == 0) {
    *i = t.val1;
    *d = t.val2;
    return true;
  }

  *i = -1;
  *d = -1;
  return false;

}

bool bme_get_temperature(int32_t *i, int32_t *d) {
  return bme_sensor_get(SENSOR_CHAN_AMBIENT_TEMP, i, d);
}

bool bme_get_pressure(int32_t *i, int32_t *d) {
  return bme_sensor_get(SENSOR_CHAN_PRESS, i, d);
}

bool bme_get_humidity(int32_t *i, int32_t *d) {
  return bme_sensor_get(SENSOR_CHAN_HUMIDITY, i, d);
}

