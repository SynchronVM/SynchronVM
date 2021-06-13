
/* SenseVM Chibios hardware configuration and mapping file */
/* Should not be included into more than one C file (included in gpio.c per default) */

#ifndef SVM_CHIBIOS_CONF_H_
#define SVM_CHIBIOS_CONF_H_

gpio_pad_t leds[2] =
  {{GPIOD, 13, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST},
   {GPIOD, 12, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST}};


gpio_button_t buttons[1] =
  {{GPIOA, 0, PAL_MODE_INPUT, PAL_EVENT_MODE_BOTH_EDGES}};

#endif 
