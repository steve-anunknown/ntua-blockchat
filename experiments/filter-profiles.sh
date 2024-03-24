#!/bin/bash

# make grep not print color codes
export GREP_OPTIONS='--color=never'
pattern="processTXs' \|validateTransaction \|        nodeLogic.mint' \|txIsUnique"

directory="profiled_outputs/docker"

NODE="1"
FUNCTION="2"
MODULE="3"
SOURCE="4"
NUMBER="5"
ENTRIES="6"
TIME_IND="7"
MEM_IND="8"
TIME_INH="9"
MEM_INH="10"

profile_to_csv() {
    # take raw info that has been pattern matched with grep
    # and convert it to a csv file
    local infofile="$1"
    local output="$2"
    cp "$infofile" "$output"
    sed -i '1s/^/Node,Function,Module,Source,Number,Entries,TimeInd,MemInd,TimeInh,MemInh\n/' "$output"    
    sed -i 's/ \+/ /g' "$output"
    sed -i 's/.*\/node/node/' "$output"
    sed -i 's/\.prof\.prof:/\.prof/' "$output"
    sed -i 's/ /,/g' "$output"
    # find all commas that lie within parentheses and replace them with -
    sed -i 's/\(([^,]*\),\([^,)]*\))/\1-\2/g' "$output"
}

for experiment in "$directory"/*;
do
    echo "Processing $experiment"
    for folder in "$experiment"/*;
    do
        # search the file from the second "COST CENTRE" to the end
        grep "$pattern" "$folder"/*.prof > "$folder"/rawinfo.txt
        profile_to_csv "$folder"/rawinfo.txt "$folder"/rawinfo.csv

        grep "total time" "$folder"/*.prof > "$folder"/total-times.txt
        sed -i 's/.*\///' "$folder"/total-times.txt
        sed -i 's/ \+/ /g' "$folder"/total-times.txt

        cp "$folder"/total-times.txt "$folder"/total-times.csv
        sed -i 's/total time = //g' "$folder"/total-times.csv
        sed -i 's/\.prof:/\.prof,/g' "$folder"/total-times.csv
        sed -i 's/ /,/g' "$folder"/total-times.csv
        sed -i '1s/^/Node,TotalTime,Unit,Ticks\n/' "$folder"/total-times.csv

        grep "txIsUnique" "$folder"/rawinfo.csv | cut -d',' -f${FUNCTION},${ENTRIES} > "$folder"/txIsUnique.csv

        sed -i 's/.*\///' "$folder"/txIsUnique.csv
        sed -i 's/ \+/ /g' "$folder"/txIsUnique.csv
        # append a line that sums the elements of the second column
        # awk "{s+=${FUNCTION}} END {print \"Total validated txs = \" s}" "$folder"/txIsUnique.csv >> "$folder"/txIsUnique.csv
        grep "mint" "$folder"/rawinfo.csv | cut -d',' -f${FUNCTION},${ENTRIES} > "$folder"/mint.csv

        total_time=$(cut -d',' -f2 "$folder"/total-times.csv | awk '{s+=$1} END {print s}')
        total_txs=$(cut -d',' -f2 "$folder"/txIsUnique.csv | awk '{s+=$1} END {print s}')
        throughput=$(echo "scale=2; $total_txs / $total_time" | bc)
        # the total blocks are the maximum number of entries in the mint file
        total_blocks=$(cut -d',' -f2 "$folder"/mint.csv | sort -n | tail -n 1)
        blocktime=$(echo "scale=2; $total_time" / "$total_blocks" | bc)
        final_line="Time = $total_time, TXs = $total_txs, Blocks = $total_blocks  => Throughput = $throughput, Blocktime = $blocktime"

        paste "$folder"/total-times.txt "$folder"/txIsUnique.csv "$folder"/mint.csv > "$folder"/final.csv
        sed -i 's/,/ = /g' "$folder"/final.csv
        sed -i 's/(.*)//g' "$folder"/final.csv
        echo "$final_line" >> "$folder"/final.csv
    done
done
