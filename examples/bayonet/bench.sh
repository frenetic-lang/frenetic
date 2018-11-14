#!/bin/bash

# Requires the following environment variables to be set:
# * CMD: the command to benchmark
# * NAME: name of this benchmark
# 
# Additionally, one can set the following optional variable:
# * F=1: Rerun benchmark, even if result file already exists.
# * TIMEOUT: timeout in seconds (defaults to 1h)

set -o pipefail

function bench {
    # check that parameters were given
  if [[ -z $CMD || -z $NAME ]]; then
    echo "environment variables CMD and NAME must be set"
    exit 1
  fi

  # default values
  TIMEOUT=${TIMEOUT:-3600} # default to 1h
  RESULT_FILE="$NAME.log"

  if [[ ! -e $RESULT_FILE || -n $F ]]; then
    echo "$ $CMD" | tee $RESULT_FILE
    { { time timeout $TIMEOUT $CMD; } 2>&1; } | tee -a $RESULT_FILE
    RC="$?"
    tail $RESULT_FILE
    exit $RC
  else
    echo "Skipping ${RESULT_FILE}: it already exists. (Run with F=1 to force rerun)."
    tail $RESULT_FILE
    exit 0
  fi
}