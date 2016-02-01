#!/bin/bash

../../../frenetic.native dump virtual vpol.kat

for arg in "g_fabric" "g_pruned" "pg" "vg" "g_raw" 
do 
  echo $arg
  arg1="${arg}.dot"
  arg2="${arg}.png"
  dot -Tpng $arg1 > $arg2
done
