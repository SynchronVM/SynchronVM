
all: button_blinky.cam
	camiotc button_blinky.cam -o button_blinky.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm button_blinky.svm
	cd src-chibios; make clean
