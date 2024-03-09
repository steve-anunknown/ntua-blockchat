#!/bin/bash

# make grep not print color codes
export GREP_OPTIONS='--color=never'
pattern="processTXs' \|validateTransaction \|        nodeLogic.mint' "

directory="profiled_outputs"

for experiment in "$directory"/*;
do
    # check that folder starts with "capacity"
    echo "Processing $experiment"
    for folder in "$experiment"/*;
    do
        grep "$pattern" "$folder"/*.prof.prof > "$folder"/calls.txt
    done
done
