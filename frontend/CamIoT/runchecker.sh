#!/bin/sh

success_count=0
fail_count=0;
failing_tests="failed tests: \n"

echo "********************"
echo ""
echo "running good tests"
echo ""
echo "********************"

#for file in `ls src/Typechecker/testprograms/`
#do
#	if [ $(basename $file) != $(basename $0) ]
#        then
#		var=$(CamIoT-exe $file 2>&1)
#		res=$?
#	        if [ "$res" -eq 0 ]
#                then
#		    success_count=$((success_count+1))
#		    echo "file: $file typechecked successfully"
#	        fi
#		if [ "$res" -eq 1 ]
#		then
#		    fail_count=$((fail_count+1))
#		    failing_tests="$failing_tests $file \n"
#		    echo "\noops!"
#		    echo "file: $file did not parse successfully"
#		    echo $var
#			echo ""
#		fi
#        fi
#done

echo "********************"
echo ""
echo "running failing datatype declaration tests"
echo ""
echo "********************"

for file in `ls src/Typechecker/testprograms/bad-datatype-decl`
do
	if [ $(basename $file) != $(basename $0) ]
        then
		var=$(camiotc src/Typechecker/testprograms/bad-datatype-decl/$file 2>&1)
		res=$?
	        if [ "$res" -ne 0 ]
                then
		    success_count=$((success_count+1))
		    echo "file: $file failed typechecking, as expected"
	        fi
		if [ "$res" -eq 0 ]
		then
		    fail_count=$((fail_count+1))
		    failing_tests="$failing_tests $file \n"
		    echo "\noops!"
		    echo "file: $file passed typechecking, which was unexpected..."
		    echo $var
			echo ""
		fi
        fi
done

echo "********************"
echo ""
echo "running working datatype declaration tests"
echo ""
echo "********************"

for file in `ls src/Typechecker/testprograms/good-adt-tests`
do
	if [ $(basename $file) != $(basename $0) ]
        then
		var=$(camiotc src/Typechecker/testprograms/good-adt-tests/$file 2>&1)
		res=$?
	        if [ "$res" -eq 0 ]
                then
		    success_count=$((success_count+1))
		    echo "file: $file passed typechecking, as expected"
	        fi
		if [ "$res" -ne 0 ]
		then
		    fail_count=$((fail_count+1))
		    failing_tests="$failing_tests $file \n"
		    echo "\noops!"
		    echo "file: $file failed typechecking, which was unexpected..."
		    echo $var
			echo ""
		fi
        fi
done

echo "********************"
echo ""
echo "running bad tests"
echo ""
echo "********************"

for file in `ls src/Typechecker/testprograms/bad-typechecks/`
do
	if [ $(basename $file) != $(basename $0) ]
        then
		var=$(camiotc src/Typechecker/testprograms/bad-typechecks/$file 2>&1)
		res=$?
	        if [ "$res" -eq 2 ]
                then
		    success_count=$((success_count+1))
		    echo "file: $file failed typechecking, as expected"
	        fi
		if [ "$res" -ne 2 ]
		then
		    fail_count=$((fail_count+1))
		    failing_tests="$failing_tests $file \n"
		    echo "\noops!"
		    echo "file: $file passed typechecking, which was unexpected..."
		    echo $var
			echo ""
		fi
        fi
done

echo ""
echo Tests passed: $success_count
echo Tests failed: $fail_count

if [ $fail_count -gt 0 ]
then
    echo $failing_tests
    exit 1
fi
