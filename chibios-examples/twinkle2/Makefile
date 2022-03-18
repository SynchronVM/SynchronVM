
all: twinkle.cam
	camiotc twinkle.cam -o twinkle.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

debug:
	cd src-chibios; ./debug.sh


clean:
	rm twinkle.svm
	cd src-chibios; make clean
