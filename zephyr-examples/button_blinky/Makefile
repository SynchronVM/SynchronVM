

all:
	camiotc button_blinky.cam -o button_blinky.svm
	west build -p auto -b nrf52840dk_nrf52840 .

module:
	camiotc button_blinky.cam -o button_blinky.svm
	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../

stm:
	west build -p auto -b stm32f4_disco .
clean:
	rm -r build/
