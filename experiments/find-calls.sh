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
        # search the file from the second "COST CENTRE" to the end
        grep "$pattern" "$folder"/*.prof.prof > "$folder"/calls.txt
        # replace every occurence of multiple spaces with a comma
        # followed by a space
        sed -i 's/ \+/,\ /g' "$folder"/calls.txt
        sed -i "s/${directory}\///g" "$folder"/calls.txt
        # delete the pattern that starts with 'src' and
        # ends with '),'.
        sed -i 's/src.*),//g' "$folder"/calls.txt
        # if there is a line with ':' followed by an alphabetical
        # character, delete it
        sed -i '/:[a-zA-Z]/d' "$folder"/calls.txt
    done
done
