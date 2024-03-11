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



        # create a new file called calls-latex.txt
        cp "$folder"/calls.txt "$folder"/calls-latex.txt
        # every line starts with the path of the file
        # delete it and keep only the node<num>.prof
        sed -i 's/.*\///' "$folder"/calls-latex.txt
        # match node<num>.prof.prof and replace it
        # with node<num>.prof
        sed -i 's/.prof.prof:/.prof/' "$folder"/calls-latex.txt
        # prepend to the first line of the file
        # the string "File, Function, Module, Number, Calls, Time-Ind, Mem-Ind, Time-Inh, Mem-Inh"
        sed -i '1s/^/File, Function, Module, Number, Calls, TimeInd, MemInd, TimeInh, MemInh\n/' "$folder"/calls-latex.txt
        sed -i "s/nodeLogic\.//g" "$folder"/calls-latex.txt
        sed -i "s/processTXs\.//g" "$folder"/calls-latex.txt
        # keep only columns 1, 2, 5, 8, 9
        awk -F, '{print $1","$2","$5","$8","$9}' "$folder"/calls-latex.txt > "$folder"/calls-latex.csv
    done
done
