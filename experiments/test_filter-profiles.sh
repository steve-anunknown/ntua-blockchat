# Test 1: Verify total_time calculation
echo "Test 1: Verify total_time calculation"
input="10,20,30,40\n10,80,30,40"
expected_output="100"
actual_output=$(echo "$input" | cut -d',' -f2 | awk '{s+=$1} END {print s}')
if [ "$actual_output" = "$expected_output" ]; then
    echo "Pass"
else
    echo "Fail"
fi

# Test 2: Verify total_txs calculation
echo "Test 2: Verify total_txs calculation"
input="5,10,15,20\n5,40,15,20"
expected_output="50"
actual_output=$(echo "$input" | cut -d',' -f2 | awk '{s+=$1} END {print s}')
if [ "$actual_output" = "$expected_output" ]; then
    echo "Pass"
else
    echo "Fail"
fi