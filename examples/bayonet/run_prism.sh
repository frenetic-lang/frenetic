#!/bin/bash

TIMEOUT=3600
PRISM="prism/prism-4.4/prism/bin/prism"

if [[ -z $1 ]]; then
  echo "usage: $0 <TOPOLOGY FACTOR k>"
  exit 1
fi

K="$1"
SW=$(expr 4 \* ${K})
RESULT_FILE="bayonet_resilience_sw_${SW}.prism.log"
CMD="$PRISM prism/bayonet.pm prism/bayonet.pctl -const k=${K} -exact"

if [ ! -f $RESULT_FILE ]; then
  { { time timeout $TIMEOUT $CMD; } 2>&1; } > $RESULT_FILE
  RC="$?"
  tail $RESULT_FILE
  exit $RC
else
  echo "skipping ${RESULT_FILE}; already exists."
  tail $RESULT_FILE
fi
