
all: conc.cam
	camiotc conc.cam -o conc.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm conc.svm
	cd src-chibios; make clean
