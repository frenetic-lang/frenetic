#!/bin/bash


if [[ -z $1 ]]; then
  echo "usage: $0 <k>"
  exit 1
fi

K=$1
PRISM="prism"

source params.sh
PNK="probnetkat.with_topo -fail-with ${FAILURE_PROB} -scheme ECMP -cps -fbound ${FAILURE_BOUND} -prism"
PRISM_FILE="$K.pm"
SW=$(( ($K*$K*5)/4 ))
TOPO_FILE="../../../examples/output/fattree_${K}_sw_${SW}.dot"
NAME="ecmp_fat${K}_fb${FAILURE_BOUND}.prism.compiled"
CMD="$PRISM $PRISM_FILE prism.pctl -maxiters 1000000"


# SJS: it would be better to make this part of the CMD, but that's tricky.
$PNK $TOPO_FILE > $PRISM_FILE
source bench.sh
bench
