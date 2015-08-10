#!/bin/bash

../../../frenetic.native virtual_cmd\
  mapreduce/vpol.kat\
  mapreduce/vrel.kat\
  mapreduce/vtopo.kat\
  mapreduce/vingpol.kat\
  mapreduce/ving.kat\
  mapreduce/veg.kat\
  ptopo.kat\
  ping.kat\
  peg.kat


for arg in "g_fabric" "g_pruned" "pg" "vg" "g_raw" 
do 
  echo $arg
  arg1="${arg}.dot"
  arg2="${arg}.png"
  dot -Tpng $arg1 > $arg2
done
