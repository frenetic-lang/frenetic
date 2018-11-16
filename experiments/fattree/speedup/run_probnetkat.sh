#!/bin/bash

if [[ -z $1 ]]; then
  echo "usage: $0 <k>"
  exit 1
fi

PNK="probnetkat.with_topo -fail-with 1/1000 -scheme ECMP -cps"
K=$1
J=${J:-1} # local jobs
RJ=${RJ:-1} # remote jobs per remote machine
RN=${RN:-0} # remote workers - none by default
SW=$(( ($K*$K*5)/4 ))
TOPO_FILE="../../../examples/output/fattree_${K}_sw_${SW}.dot"

NAME="ecmp_fat${K}.probnetkat_j${J}_rj${RJ}_rn${RN}"
CMD="$PNK $TOPO_FILE"
export J
export RJ
export RN

source bench.sh
bench
