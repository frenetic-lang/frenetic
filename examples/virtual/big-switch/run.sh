#!/bin/bash

../../../frenetic.native virtual_cmd\
  --vpol vpol.kat\
  --vrel vrel.kat\
  --vtopo vtopo.kat\
  --ving-pol vingpol.kat\
  --ving ving.kat\
  --veg veg.kat\
  --ptopo ptopo.kat\
  --ping ping.kat\
  --peg peg.kat


for arg in "g_fabric" "g_pruned" "pg" "vg" "g_raw" 
do 
  echo $arg
  arg1="${arg}.dot"
  arg2="${arg}.png"
  dot -Tpng $arg1 > $arg2
done
