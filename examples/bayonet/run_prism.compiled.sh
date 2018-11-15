#!/bin/bash

PRISM="prism/prism-4.4/prism/bin/prism"
PNK="probnetkat.bayonet_prism"

if [[ -z $1 || ! -f $1 ]]; then
  echo "usage: $0 <dot-file>"
  exit 1
fi

INPUT=$1
PRISM_FILE="${INPUT%.dot}.pm"

NAME="${INPUT%.dot}.prism"
CMD="$PRISM $PRISM_FILE prism/bayonet.compiled.pctl"

if [[ -n $EXACT ]]; then
  NAME+="_exact.compiled"
  CMD+=" -exact"
else
  NAME+="_approx.compiled"
  CMD+=" -maxiters 1000000"
fi

# SJS: it would be better to make this part of the CMD, but that's tricky.
$PNK $INPUT > $PRISM_FILE
source bench.sh
bench
