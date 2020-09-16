#!/bin/sh

success=1
for test_file in ./tests/*.onyx ; do
	filename=$(basename -- "$test_file")
	name="${filename%.*}"

	echo "⏲ Checking $name.onyx"

	if ! ./onyx "$test_file" -o "./tests/$name.wasm" >/dev/null; then
		echo "❌ Failed to compile $name.onyx."
		success=0
		continue
	fi
	
	if ! node onyxcmd.js "./tests/$name.wasm" > ./tmpoutput; then
		echo "❌ Failed to run $name.onyx."
		success=0
		continue
	fi

	if ! diff ./tmpoutput "./tests/$name" >/dev/null; then
		echo "❌ Test output did not match."
		success=0
		continue
	fi
done
rm ./tmpoutput

([ $success = 1 ] && echo "✔ All tests passed.") \
				  || echo "❌ Some tests failed."
