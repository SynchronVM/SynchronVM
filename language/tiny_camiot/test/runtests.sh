#!/bin/sh

for file in `ls`
do
	if [ $(basename $file) != $(basename $0) ]
        then
		var=$(../grammar/./Testtiny_camiot < $file 2>&1)
		res=$?
	        if [ "$res" -eq 0 ]
                then
			echo "file: $file parsed successfully"
	        fi
		if [ "$res" -eq 1 ]
		then
			echo "\noops!"
			echo "file: $file did not parse successfully"
			echo $var
			break
		fi
        fi
done
