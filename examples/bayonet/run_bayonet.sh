#!/bin/bash
BAYONET="../../../bayonet/bayonet"
PSI="../../../psi/psi --noboundscheck --trace --dp --expectation --mathematica"
INPUT="$1"
PSI_FILE=${1%.bayonet}.psi
RESULT_FILE=${1}.log
TIMEOUT=3600

if [[ -z $1 ]]; then
  echo "usage: $0 <bayonet-file>"
  exit 1
fi

if [ ! -f $PSI_FILE ]; then
    echo compiling $INPUT to $PSI_FILE
    $BAYONET $INPUT &> $PSI_FILE
else
  echo "skipping $1; $PSI_FILE already exists."
fi

if [ ! -f $RESULT_FILE ]; then
    echo "analyzing: $PSI $PSI_FILE"
    { { time timeout $TIMEOUT $PSI $PSI_FILE; } 2>&1; } > $RESULT_FILE
    tail $RESULT_FILE
else
    echo "skipping $PSI_FILE; $RESULT_FILE already exists."
    tail $RESULT_FILE
fi
