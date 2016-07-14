#!/bin/bash
COMPILE="../../_build/bench/src/Main.native"
if [ -z "$1" ]
  then 
    echo "Must specify input file!"
  else 
    echo "Running fattree benchmark with input file \"$1\" ..." 
    $COMPILE compile "local" per-switch varorder-fattree tablegen-naive "false" $1
fi

