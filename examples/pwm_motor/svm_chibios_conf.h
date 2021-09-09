
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

#define PWM0_DRIVER PWMD3
#define PWM0_FREQ   1000000
#define PWM0_PERIOD 500

#define PWM0_CH1_GPIO GPIOC
#define PWM0_CH1_PIN  7
#define PWM0_CH1_MODE PAL_MODE_ALTERNATE(2) | \
                      PAL_STM32_OSPEED_HIGHEST | \
                      PAL_STM32_PUPDR_FLOATING

#define BUTTON0_GPIO GPIOA
#define BUTTON0_PIN  0
#define BUTTON0_MODE PAL_MODE_INPUT
#define BUTTON0_EVENT_MODE PAL_EVENT_MODE_BOTH_EDGES

#define SYS_TIMER  5 /*select tim5 for system time*/

#endif 