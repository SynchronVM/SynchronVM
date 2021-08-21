
all: tripple.cam
	camiotc tripple.cam -o tripple.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm tripple.svm
	cd src-chibios; make clean
