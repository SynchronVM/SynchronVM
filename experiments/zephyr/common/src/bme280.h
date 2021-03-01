#ifndef BME280_H_
#define BME280_H_

/* ******* */
/* BME 280 */

#define BME280 DT_INST(0, bosch_bme280)

#if DT_NODE_HAS_STATUS(BME280, okay)
#define BME280_LABEL DT_LABEL(BME280)
#else
#error Your devicetree has no enabled nodes with compatible "bosch,bme280"
#define BME280_LABEL "<none>"
#endif


extern bool bme_init(void);
extern bool bme_sample(void);
extern bool bme_get_temperature(int32_t *i, int32_t *d);
extern bool bme_get_pressure(int32_t *i, int32_t *d);
extern bool bme_get_humidity(int32_t *i, int32_t *d);

#endif 
