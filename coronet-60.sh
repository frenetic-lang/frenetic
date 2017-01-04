#!/bin/bash

EX_DIR="examples/coronet"
FSH="./fresh.native"

for i in {1..10}
do
    echo "Running with $i nodes on each coast\n"
    cat $EX_DIR/coronet-60-$i.ash | $FSH >> results.txt
done

echo "EdgeNodes	Formulation(msecs)	Solution(msecs)	Generation(msecs)"
cat results.txt | grep "\*\*\*" | tr -d "***"

