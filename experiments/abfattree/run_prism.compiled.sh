#!/bin/bash

PRISM="prism"
PNK="probnetkat.with_topo -fail-with 1/1000 -scheme f10 -prism"

if [[ -z $1 ]]; then
  echo "usage: $0 <k>"
  exit 1
fi

K=$1
PRISM_FILE="$K.pm"
SW=$(( ($K*$K*5)/4 ))
TOPO_FILE="../../examples/output/abfattree_${K}_sw_${SW}.dot"

NAME="f10_ab${K}.prism.compiled"
CMD="$PRISM $PRISM_FILE prism.pctl -maxiters 1000000"


# SJS: it would be better to make this part of the CMD, but that's tricky.
$PNK $TOPO_FILE > $PRISM_FILE
source bench.sh
bench
