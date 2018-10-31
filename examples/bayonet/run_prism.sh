#!/bin/bash
BAYONET="../../../bayonet/bayonet"
PSI="../../../psi/psi --noboundscheck --trace --dp --expectation --mathematica"
INPUT="$1"
PSI_FILE=${1%.bayonet}.psi
RESULT_FILE=${1}.log
TIMEOUT=3600

if [[ -z $1 ]]; then
  echo "usage: $0 <TOPOLOGY FACTOR k>"
  exit 1
fi

prism prism/bayonet.pm prism/bayonet.pctl -const k=$1 -exact