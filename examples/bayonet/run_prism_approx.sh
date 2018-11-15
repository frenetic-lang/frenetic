#!/bin/bash

PRISM="prism/prism-4.4/prism/bin/prism"

if [[ -z $1 ]]; then
  echo "usage: $0 <TOPOLOGY FACTOR k>"
  exit 1
fi

SW="$1"
K=$(( $SW / 4 ))
NAME="bayonet_resilience_sw_$SW.prism_approx"
CMD="$PRISM prism/bayonet.pm prism/bayonet.pctl -const k=${K} -maxiters 1000000"

source bench.sh
bench
