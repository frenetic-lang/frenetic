#!/bin/bash

if [[ -z $1 ]]; then
  echo "usage: $0 <TOPOLOGY FACTOR k>"
  exit 1
fi

K="$1"
SW=$(expr 4 \* ${K})
RESULT_FILE="bayonet_resilience_sw_${SW}.prism.log"
TIMEOUT=3600
CMD="prism prism/bayonet.pm prism/bayonet.pctl -const k=${K} -exact"

{ { time timeout $TIMEOUT $CMD; } 2>&1; } > $RESULT_FILE
tail $RESULT_FILE
