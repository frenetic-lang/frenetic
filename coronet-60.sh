#!/bin/bash

EX_DIR="examples/coronet"
FSH="./fresh.native"

for i in {1..4}
do
    cat $EX_DIR/coronet-30-$i.ash | $FSH >> results.txt
done

echo "EdgeNodes	Formulation(msecs)	Solution(msecs)	Generation(msecs)"
cat results.txt | grep "\*\*\*" | tr -d "***"

