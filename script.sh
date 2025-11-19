#!/bin/bash

for i in {1..10}; do
    echo "Running test $i..."
    ./hw1 < tests/t${i}.in > tests/t${i}.res 2>&1
    diff -u tests/t${i}.res tests/t${i}.out
done

