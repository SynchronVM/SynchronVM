
/* SenseVM Chibios hardware configuration and mapping file */
/* Should not be included into more than one C file (included in gpio.c per default) */

#ifndef SVM_CHIBIOS_CONF_H_
#define SVM_CHIBIOS_CONF_H_


#define LED0_GPIO GPIOD
#define LED0_PIN  13
#define LED0_MODE PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST

#define LED1_GPIO GPIOD
#define LED1_PIN  12
#define LED1_MODE PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST

#define DAC0_CH   1
#define DAC0_GPIO GPIOA
#define DAC0_PIN  4
#define DAC0_MODE PAL_MODE_INPUT_ANALOG

#define DAC1_GPIO GPIOA
#define DAC1_PIN 5
#define DAC1_MODE PAL_MODE_INPUT_ANALOG

#define BUTTON0_GPIO GPIOA
#define BUTTON0_PIN  0
#define BUTTON0_MODE PAL_MODE_INPUT
#define BUTTON0_EVENT_MODE PAL_EVENT_MODE_BOTH_EDGES

#define SYS_TIMER  5 /*select tim5 for system time*/

#endif 
