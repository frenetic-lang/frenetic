#!/bin/bash
COMPILE="../../_build/bench/src/Main.native"
if [ -z "$1" ]
  then 
    echo "Must specify input file!"
    echo "(you may have to \"make download\" first)"
  else 
    echo "Running SDX benchmark with input file \"$1\" ..." 
    $COMPILE compile sdx $1
  fi

