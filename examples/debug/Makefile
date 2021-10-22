
all: dac_square_wave.cam
	camiotc dac_square_wave.cam -o dac_square_wave.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

debug:
	cd src-chibios; ./debug.sh


clean:
	rm dac_square_wave.svm
	cd src-chibios; make clean
