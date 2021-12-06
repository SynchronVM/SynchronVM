
all:
	camiotc button_blinky.cam -o button_blinky.svm
	west build -p auto -b nrf52840dk_nrf52840 .

stm:
	west build -p auto -b stm32f4_disco .
clean:
	rm -r build/
