#!/bin/bash

# make grep not print color codes
export GREP_OPTIONS='--color=never'
pattern="processTXs' \|validateTransaction \|        nodeLogic.mint' \|txIsUnique"

directory="profiled_outputs"

for experiment in "$directory"/*;
do
    echo "Processing $experiment"
    for folder in "$experiment"/*;
    do
        # search the file from the second "COST CENTRE" to the end
        grep "$pattern" "$folder"/*.prof.prof > "$folder"/calls.txt
        # replace every occurence of multiple spaces with a comma
        # followed by a space
        sed -i "s/${directory}\///g" "$folder"/calls.txt
        sed -i 's/ \+/,\ /g' "$folder"/calls.txt
        # delete the pattern that starts with 'src'
        # is followed by whatever and ends with '),'.
        sed -i 's/src.*),//g' "$folder"/calls.txt
        # delete the pattern src/Block.hs:132:1-52
        sed -i 's/src\/Block\.hs:132:1-52,//g' "$folder"/calls.txt
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
        grep "total time" "$folder"/*.prof.prof > "$folder"/total-times.txt
        sed -i 's/.*\///' "$folder"/total-times.txt
        sed -i 's/.prof.prof/.prof/' "$folder"/total-times.txt
        # replace multiple spaces after = with a single space
        sed -i 's/ = \+/ = /g' "$folder"/total-times.txt
        # append a line that sums up the total time
        awk '{s+=$5} END {print "Total time = " s}' "$folder"/total-times.txt >> "$folder"/total-times.txt

        grep "txIsUnique" "$folder"/*.prof.prof > "$folder"/txIsUnique.txt
        # replace multiple spaces after = with a single space
        sed -i 's/ \+/ /g' "$folder"/txIsUnique.txt
        cut -d' ' -f2,6 "$folder"/txIsUnique.txt > "$folder"/txIsUnique.csv
        # append a line that sums the elements of the second column
        awk '{s+=$2} END {print "Total validated txs = " s}' "$folder"/txIsUnique.csv >> "$folder"/txIsUnique.csv

        paste "$folder"/total-times.txt "$folder"/txIsUnique.csv > "$folder"/summary.txt
        # calculate the throughput by dividing the total time by the total validated txs
        read -r totalTime totalTxs <<< "$(awk '/^Total time/ {print $4, $9}' "$folder"/summary.txt)"
        throughput=$(echo "scale=2; $totalTxs / $totalTime" | bc)
        sed -i "$ s/$/ => Throughput = $throughput/" "$folder"/summary.txt
        sed -i 's/(.*)//' "$folder"/summary.txt
        sed -i 's/txIsUnique/Validated txs/' "$folder"/summary.txt

        grep "txIsUnique" "$folder"/*.prof.prof > "$folder"/txIsUnique.txt
        # replace multiple spaces after = with a single space
        sed -i 's/ \+/ /g' "$folder"/txIsUnique.txt
        cut -d' ' -f2,6 "$folder"/txIsUnique.txt > "$folder"/txIsUnique.csv
        # append a line that sums the elements of the second column
        awk '{s+=$2} END {print "Total validated txs = " s}' "$folder"/txIsUnique.csv >> "$folder"/txIsUnique.csv


    done
done
