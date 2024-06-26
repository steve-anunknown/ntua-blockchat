#!/bin/bash

LOCALHOST="127.0.0.1"
PORT="35900"

stack clean
stack build

capacity=("5" "10" "20" "1")
initial_stake="stake 10"

for test in "throughput" "scalability";
do
    if [ "$test" = "throughput" ]; then
        nodes="5"
    elif [ "$test" = "scalability" ]; then
        nodes="10"
    else
        echo "Invalid test"
        exit 1
    fi
    input="input${nodes}"
    msg="Running $test test"
    for cap in "${capacity[@]}";
    do
        workdir="local/harmonic/${test}/capacity${cap}"
        if [ ! -d "$workdir" ]; then
            mkdir -p "$workdir"
        fi

        echo "$msg with capacity $cap"
        input="input${nodes}"
        
        command="stack run -- --bootstrap $LOCALHOST $PORT $nodes"
        echo -e "\t$command"
        $command &

        sleep 2
        {
            time for i in $(seq 1 $nodes); do
                stdout_log="$workdir/node${i}_stdout.log"
                stderr_log="$workdir/node${i}_stderr.log"

                command="stack run -- --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap"
                echo -e "\t$command"
                (cat <(echo "$initial_stake") <(echo "load $input/trans${i}.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
                sleep 1
            done
            wait
        } 2> "$workdir/time.log"
    done
done

workdir="local/harmonic/fairness"
# Run the fairness test
if [ ! -d "$workdir" ]; then
    mkdir -p "$workdir"
fi

nodes="5"
cap="5"
unfair_stake="stake 100"
input="input${nodes}"

echo "Running fairness test with capacity $cap"
command="stack run -- --bootstrap $LOCALHOST $PORT $nodes"
echo -e "\t$command"
$command &
sleep 1


workdir="${workdir}/capacity${cap}"
if [ ! -d "$workdir" ]; then
    mkdir "$workdir"
fi
stdout_log="$workdir/node1_stdout.log"
stderr_log="$workdir/node1_stderr.log"

command="stack run -- --node $LOCALHOST $((PORT + 1)) $LOCALHOST $((PORT)) $cap"
echo -e "\t$command"
(cat <(echo "$unfair_stake") <(echo "load $input/trans1.txt") <(echo "balance") | $command) > "$stdout_log" 2> "$stderr_log" &

{
    time for i in $(seq 2 $nodes); do
        stdout_log="$workdir/node${i}_stdout.log"
        stderr_log="$workdir/node${i}_stderr.log"

        command="stack run -- --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap"
        echo -e "\t$command"
        (cat <(echo "$initial_stake") <(echo "load input${nodes}/trans${i}.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
        sleep 0.1
    done
    wait
} 2> "$workdir/time.log"


