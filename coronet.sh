#!/bin/bash

EX_DIR="examples/coronet"
FSH="./frenetic.native autoshell"

echo "Edge Nodes	Pre-processing(msecs)	Generation(msecs)	Solution(secs)"

for i in {1..4}
do
    cat $EX_DIR/coronet-30-$i.ash | $FSH | grep "|" | tr -d "|"
done


