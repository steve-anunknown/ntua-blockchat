#!/bin/bash

# time the hashing of the random string 10 times
# and get the average time
lengths="16 32 64 128 256 512 1024 2048"
repetitions=10
for i in $lengths;
do
    random_string=$(date +%s%N | sha256sum | head -c "$i")
    echo "Hashing a string of length $i"
    avg_time=0
    for j in $(seq 1 $repetitions);
    do
        start=$(date +%s%N)
        echo "$random_string" | sha256sum
        end=$(date +%s%N)
        avg_time=$((avg_time + (end - start)))
    done
    echo "Average time: $avg_time nanoseconds"
done
