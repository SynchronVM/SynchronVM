
all: blinky.cam
	camiotc blinky.cam -o blinky.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm blinky.svm
