#!/bin/bash
COMPILE="../../_build/bench/src/Main.native"
if [ -z "$1" ]
  then 
    echo "Must specify input file!"
    echo "(may have to download files first by executing download.sh)"
  else 
    echo "Running SDX benchmark with input file \"$1\" ..." 
    $COMPILE compile sdx $1
  fi

