#!/bin/bash

EX_DIR="examples/coronet"
FSH="./fresh.native"

echo "EdgeNodes	Formulation(msecs)	Write(msecs)	Solution(msecs)	Read(msecs)	Generation(msecs)	Rules"

for i in {1..4}
do
    cat $EX_DIR/coronet-30-$i.ash | $FSH | grep "|" | tr -d "|"
done


