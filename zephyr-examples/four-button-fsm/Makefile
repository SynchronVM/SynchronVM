
all:
	camiotc button_fsm.cam -o button_fsm.svm
	west build -p auto -b nrf52840dk_nrf52840 .

clean:
	rm -r build/
	rm button_fsm.svm
