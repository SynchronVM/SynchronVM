
#include <gpio.h>

#include <svm_chibios_conf.h> 

uint32_t gpio_num_leds(void) {
  return sizeof(leds);
}

uint32_t gpio_num_buttons(void) {
  return sizeof(buttons);
}
