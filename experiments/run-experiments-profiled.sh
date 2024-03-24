#!/bin/bash

LOCALHOST="127.0.0.1"
PORT="35900"
EXEC="BlockChat-exe"
PREFIX="profiled_outputs"
SUFFIX="_bytes" # <- modify this if you want to add a suffix to the output directory to indicate a different experiment

# Compile the program with profiling enabled (and optimized)
stack clean &&
stack build --profile || exit 1

initial_stake="stake 10"
nodes="5"
capacity=("5" "10" "20")
for test in "throughput"; do
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
    for cap in "${capacity[@]}"; do

        workdir="${PREFIX}/${test}${SUFFIX}/capacity${cap}"
        if [ ! -d "$workdir" ]; then
            mkdir -p "$workdir"
        fi
        echo "$msg with capacity $cap"

        command="stack exec --profile -- $EXEC --bootstrap $LOCALHOST $PORT $nodes +RTS -p -RTS"
        echo -e "\t$command"
        $command &
        sleep 1
        {
            time for i in $(seq 1 $nodes); do
                stdout_log="$workdir/node${i}_stdout.log"
                stderr_log="$workdir/node${i}_stderr.log"
                prof_log="$workdir/node${i}.prof"  # profiling output file
                command="stack exec --profile -- $EXEC --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap +RTS -p -po${prof_log} -RTS"
                echo -e "\t$command"
                (cat <(echo "$initial_stake") \
                     <(echo "load input${nodes}/trans${i}.txt") \
                     <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
                sleep 1
            done
            wait
        } 2> "$workdir/time.log"
        # if stderrs are empty delete them
        # for i in $(seq 1 $nodes); do
        #     stderr_log="$workdir/node${i}_stderr.log"
        #     if [ ! -s "$stderr_log" ]; then
        #         rm "$stderr_log"
        #     fi
        # done
    done
done

# Run the fairness test
if [ ! -d "${PREFIX}/fairness${SUFFIX}" ]; then
    mkdir "${PREFIX}/fairness${SUFFIX}"
fi

cap="5"
nodes="5"
unfair_stake="stake 100"
input="input${nodes}"

echo "Running fairness test with capacity $cap"
command="stack exec --profile -- $EXEC --bootstrap $LOCALHOST $PORT $nodes +RTS -p -RTS"
echo -e "\t$command"
$command &
sleep 1

workdir="${PREFIX}/fairness${SUFFIX}/capacity${cap}"
if [ ! -d "$workdir" ]; then
    mkdir "$workdir"
fi
stdout_log="$workdir/node1_stdout.log"
stderr_log="$workdir/node1_stderr.log"
prof_log="$workdir/node1.prof"  # profiling output file

command="stack exec --profile -- $EXEC --node $LOCALHOST $((PORT + 1)) $LOCALHOST $((PORT)) $cap +RTS -p -po${prof_log} -RTS"
echo -e "\t$command"
(cat <(echo "$unfair_stake") <(echo "load $input/trans1.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &

{
    time for i in $(seq 2 $nodes); do
        stdout_log="$workdir/node${i}_stdout.log"
        stderr_log="$workdir/node${i}_stderr.log"
        prof_log="$workdir/node${i}.prof"  # profiling output file

        command="stack exec --profile -- $EXEC --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap +RTS -p -po${prof_log} -RTS"
        echo -e "\t$command"
        (cat <(echo "$initial_stake") \
             <(echo "load input${nodes}/trans${i}.txt") \
             <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
        sleep 1
    done
    wait
} 2> "$workdir/time.log"
# if stderrs are empty delete them
for i in $(seq 1 $nodes); do
    stderr_log="$workdir/node${i}_stderr.log"
    if [ ! -s "$stderr_log" ]; then
        rm "$stderr_log"
    fi
done


