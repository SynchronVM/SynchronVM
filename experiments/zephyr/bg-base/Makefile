

all:
	west -v build  -p auto -b nrf52840dk_nrf52840 .


dev_kit:
	west build -p auto -b nrf52840dk_nrf52840 .

nrf_mod:
	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../ .

base:
	west build -p auto -b octopi_iot_board -- -DBOARD_ROOT=../ .

bg95_base:
	west build -p auto -b bg95_iot_board -- -DBOARD_ROOT=../ .


discovery:
	west build -p auto -b stm32f4_disco


stm_flash:
	openocd -f interface/stlink.cfg -f target/stm32f4x.cfg -c "init" -c "program build/zephyr/zephyr.hex verify reset exit"

nrf_flash:
	openocd -f interface/stlink.cfg -f target/nrf52.cfg -c "init" -c "program build/zephyr/zephyr.hex verify reset exit"

clean:
	rm -r build/
