#!/bin/bash

EX_DIR="examples/coronet"
FSH="./frenetic.native autoshell"

echo "Edge Nodes	Pre-processing(msecs)	Generation(msecs)	Solution(secs)	Rules"

for i in {1..10}
do
    cat $EX_DIR/coronet-60-$i.ash | $FSH | grep "|" | tr -d "|"
done


