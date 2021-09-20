
# Platform dependent subsystem for SenseVM



## Example trees



```
chibios/
├── include
│   ├── button.h
│   ├── dac.h
│   ├── gpio.h
│   ├── led.h
│   ├── platform_gpio_printf.h
│   ├── platform.h
│   ├── pwm.h
│   ├── svm_chibios.h
│   └── usbcfg.h
└── src
    ├── button.c
    ├── dac.c
    ├── gpio.c
    ├── led.c
    ├── pwm.c
    ├── svm_chibios.c
    ├── svm_chibios_main.c
    ├── sys_time.c
    └── usbcfg.c
```


```
zephyr/
├── include
│   ├── bme280.h
│   ├── button.h
│   ├── defines.h
│   ├── led.h
│   ├── ltr_303als.h
│   ├── platform_gpio.printf.h
│   ├── platform.h
│   ├── powerman.h
│   ├── svm_zephyr.h
│   ├── timerman.h
│   ├── uart.h
│   └── usb_cdc.h
└── src
    ├── bme280.c
    ├── button.c
    ├── defines.c
    ├── led.c
    ├── ltr_303als.c
    ├── powerman.c
    ├── svm_zephyr.c
    ├── sys_debug_uart.c
    ├── sys_time.c
    ├── timerman.c
    ├── uart.c
    └── usb_cdc.c
```

