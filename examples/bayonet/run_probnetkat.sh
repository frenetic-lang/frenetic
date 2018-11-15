#!/bin/bash

if [[ -z $1 || ! -f $1 ]]; then
  echo "usage: $0 <dot-file>"
  exit 1
fi

PNK="probnetkat.bayonet"
INPUT=$1
CPS=${CPS:-false} # default to no CPS
PAR=${PAR:-true} # default to parallel
RN=${RN:-0} # remote workers - none by default

NAME="${INPUT%.dot}.probnetkat_${CPS}_${PAR}_${RN}"
CMD="$PNK $INPUT $CPS $PAR"
export RN

source bench.sh
bench
