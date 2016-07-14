#!/bin/bash

for k in `seq 2 2 60`; do
  target=fattree-$k.json
  if [ ! -f $target ]; then
    echo "Generating policy for k=$k ..."
    $BENCH fattree-json $k $target
  fi
done
