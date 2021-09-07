
all:  pwm.cam
	camiotc pwm.cam -o pwm.svm
	cd src-chibios; make

flash:
	cd src-chibios; ./flash.sh

clean:
	rm pwm.svm
	cd src-chibios; make clean
