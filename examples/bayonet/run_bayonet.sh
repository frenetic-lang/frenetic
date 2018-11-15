#!/bin/bash

if [[ -z $1 ]]; then
  echo "usage: $0 <bayonet-file>"
  exit 1
fi

BAYONET="../../../bayonet/bayonet"
PSI="../../../psi/psi --noboundscheck --trace --dp --expectation --mathematica"

INPUT="$1"
PSI_FILE=${1%.bayonet}.psi
NAME="$INPUT"
CMD="$PSI $PSI_FILE"

if [[ ! -f $PSI_FILE ]]; then
    echo compiling $INPUT to $PSI_FILE
    $BAYONET $INPUT &> $PSI_FILE
else
  echo "skipping $1; $PSI_FILE already exists."
fi

source bench.sh
bench
