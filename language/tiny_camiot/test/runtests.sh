#!/bin/sh

success_count=0
fail_count=0;
failing_tests="failed tests: \n"

for file in `ls`
do
	if [ $(basename $file) != $(basename $0) ]
        then
		var=$(../grammar/./TestTinyCamiot < $file 2>&1)
		res=$?
	        if [ "$res" -eq 0 ]
                then
		    success_count=$((success_count+1))
		    echo "file: $file parsed successfully"
	        fi
		if [ "$res" -eq 1 ]
		then
		    fail_count=$((fail_count+1))
		    failing_tests="$failing_tests $file \n"
		    echo "\noops!"
		    echo "file: $file did not parse successfully"
		    echo $var
		fi
        fi
done

echo Tests passed: $success_count
echo Tests failed: $fail_count

if [ $fail_count -gt 0 ]
then
    echo $failing_tests
    exit 1
fi
