#!/bin/bash

if [[ -z $1 || ! -f $1 ]]; then
  echo "usage: $0 <dot file>"
  exit 1
fi

PNK="probnetkat.with_topo -fail-with 1/1000 -scheme ECMP -cps"
DOT_FILE=$1
RN=${RN:-0} # remote workers - none by default

NAME=$(basename "$DOT_FILE" .dot)
NAME+=".probnetkat"
CMD="$PNK $DOT_FILE"
export RN

source bench.sh
bench
