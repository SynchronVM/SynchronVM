
all:
	west build -p auto -b nrf52840dk_nrf52840 .

dev_kit:
	west build -p auto -b nrf52840dk_nrf52840 .

discovery:
	west build -p auto -b stm32f4_disco .

nrf_mod:
	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../../../backend/tests/hal/zephyr/ .

clean:
	rm -r build/
