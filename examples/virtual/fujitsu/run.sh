#!/bin/bash

if [[ ("$1" != "1") && ("$1" != "2") && ("$1" != "3") ]]
then
  echo 'Must specify argument "1", "2", or "3" to compile the coressponding VNO.'
  exit
fi

../../../frenetic.native dump virtual vno"$1"/vpol.kat\
  --vrel vno"$1"/vrel.kat\
  --ving-pol vno"$1"/vingpol.kat\
  --ving vno"$1"/vinout.kat\
  --veg vno"$1"/vinout.kat\
  --ping pinout.kat\
  --peg pinout.kat 


if which dot >/dev/null
then
  for arg in "g_fabric" "g_pruned" "pg" "vg" "g_raw"
  do
    echo $arg
    arg1="${arg}.dot"
    arg2="${arg}.svg"
    dot -Tsvg $arg1 > $arg2
  done
fi