

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

