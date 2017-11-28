#!/bin/bash

frenetic dump virtual vpol.kat

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
