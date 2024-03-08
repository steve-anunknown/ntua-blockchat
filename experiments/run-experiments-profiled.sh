#!/bin/bash

LOCALHOST="127.0.0.1"
PORT="35900"
EXEC="BlockChat-exe"

# Compile the program with profiling enabled
stack build --profile

# Run the throughput test
if [ ! -d "throughput_outdir_profiled" ]; then
    mkdir throughput_outdir_profiled
fi

nodes="5"
capacity=("5" "10" "20")
initial_stake="stake 10"
for cap in "${capacity[@]}"; do
    echo "Running throughput test with capacity $cap"
    command="stack exec --profile -- $EXEC --bootstrap $LOCALHOST $PORT $nodes +RTS -p"
    echo -e "\t$command"
    $command &
    sleep 1

    workdir="throughput_outdir_profiled/capacity${cap}"
    if [ ! -d "$workdir" ]; then
        mkdir "$workdir"
    fi

    {
        time for i in $(seq 1 $nodes); do
            node_workdir="$workdir/node${i}"
            if [ ! -d "$node_workdir" ]; then
                mkdir "$node_workdir"
            fi

            stdout_log="$node_workdir/stdout.log"
            stderr_log="$node_workdir/stderr.log"
            cd "$node_workdir" || exit
            command="stack exec --profile -- $EXEC --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap +RTS -p"
            echo -e "\t$command"
            (cat <(echo "$initial_stake") <(echo "load ../input${nodes}/trans${i}.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
            sleep 0.1
            cd ../../../ || exit
        done
        wait
    } 2> "$workdir/time.log"
done

exit

# Run the scalability test
if [ ! -d "scalability_outdir" ]; then
    mkdir scalability_outdir
fi

nodes="10"
input="input${nodes}"
for cap in "${capacity[@]}";
do
    echo "Running scalability test with capacity $cap"
    command="stack run -- --bootstrap $LOCALHOST $PORT $nodes"
    echo -e "\t$command"
    $command &
    sleep 1


    workdir="scalability_outdir/capacity${cap}"
    if [ ! -d "$workdir" ]; then
        mkdir "$workdir"
    fi
    {
        time for i in $(seq 1 $nodes); do
            stdout_log="$workdir/node${i}_stdout.log"
            stderr_log="$workdir/node${i}_stderr.log"
            prof_log="$workdir/node${i}.prof"  # profiling output file

            command="stack run -- +RTS -p -o $prof_log -RTS --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap"
            echo -e "\t$command"
            (cat <(echo "$initial_stake") <(echo "load input${nodes}/trans${i}.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
            sleep 0.1
        done
        wait
    } 2> "$workdir/time.log"
done


# Run the fairness test
if [ ! -d "fairness_outdir" ]; then
    mkdir fairness_outdir
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


workdir="fairness_outdir/capacity${cap}"
if [ ! -d "$workdir" ]; then
    mkdir "$workdir"
fi
stdout_log="$workdir/node1_stdout.log"
stderr_log="$workdir/node1_stderr.log"
prof_log="$workdir/node1.prof"  # profiling output file

command="stack run -- +RTS -p -o $prof_log -RTS --node $LOCALHOST $((PORT + 1)) $LOCALHOST $((PORT)) $cap"
echo -e "\t$command"
(cat <(echo "$unfair_stake") <(echo "load $input/trans1.txt") <(echo "balance") | $command) > "$stdout_log" 2> "$stderr_log" &

{
    time for i in $(seq 2 $nodes); do
        stdout_log="$workdir/node${i}_stdout.log"
        stderr_log="$workdir/node${i}_stderr.log"
        prof_log="$workdir/node${i}.prof"  # profiling output file

        command="stack run -- +RTS -p -o $prof_log -RTS --node $LOCALHOST $((PORT + i)) $LOCALHOST $((PORT)) $cap"
        echo -e "\t$command"
        (cat <(echo "$initial_stake") <(echo "load input${nodes}/trans${i}.txt") <(echo "blockchain") | $command) > "$stdout_log" 2> "$stderr_log" &
        sleep 0.1
    done
    wait
} 2> "$workdir/time.log"


