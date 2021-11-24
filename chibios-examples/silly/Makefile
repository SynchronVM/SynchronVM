
all: silly.cam
	camiotc silly.cam -o silly.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm silly.svm
	cd src-chibios; make clean
