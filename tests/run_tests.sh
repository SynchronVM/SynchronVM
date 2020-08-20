#!/bin/bash

echo "BUILDING"

make clean
make

echo "TESTING!"

success_count=0
failed_count=0
failing_tests="failed tests: \n"
result=0


for exe in *.exe; do
    ./$exe

    result=$?

    echo "------------------------------------------------------------"
    if [ $result -eq 1 ]
    then
	success_count=$((success_count+1))
	echo $exe SUCCESS
    else
	failed_count=$((fail_count+1))
	echo $exe FAILED
    fi
    echo "------------------------------------------------------------"
done

echo -e $failing_tests
echo Tests passed: $success_count
echo Tests failed: $failed_count

if [ $failed_count -gt 0 ]
then
    exit 1
fi
