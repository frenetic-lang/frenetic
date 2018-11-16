#!/bin/bash

if [[ -z $1 || ! -f $1 ]]; then
  echo "usage: $0 <dot file>"
  exit 1
fi

PRISM="prism"
PNK="probnetkat.with_topo -fail-with 1/1000 -scheme ECMP -cps -prism"
DOT_FILE=$1
RN=${RN:-0} # remote workers - none by default

NAME=$(basename "$DOT_FILE" .dot)
PRISM_FILE="$NAME.pm"
NAME+=".prism.compiled"
CMD="$PRISM $PRISM_FILE prism.pctl -maxiters 1000000"


# SJS: it would be better to make this part of the CMD, but that's tricky.
$PNK $TOPO_FILE > $PRISM_FILE
source bench.sh
bench
