#!/bin/bash
COMPILE="../../_build/bench/src/Main.native"
if [ -z "$1" ]
  then 
    echo "Must specify input file!"
    echo "(you may have to \"make build\" first)"
  else 
    echo "Running benchmark with input file \"$1\" ..."
    $COMPILE compile local big-fdd varorder-heuristic tablegen-naive "false" $1
fi