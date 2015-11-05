#!/bin/bash

../../../frenetic.native virtual_cmd\
  --vpol mapreduce/vpol.kat\
  --vrel mapreduce/vrel.kat\
  --vtopo mapreduce/vtopo.kat\
  --ving-pol mapreduce/vingpol.kat\
  --ving mapreduce/ving.kat\
  --veg mapreduce/veg.kat\
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
