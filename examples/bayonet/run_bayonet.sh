#!/bin/bash
BAYONET="../../../bayonet/bayonet"
PSI="../../../psi/psi --noboundscheck --trace --dp --expectation --mathematica"
INPUT="$1"
PSIFILE=${1%.bayonet}.psi
RESFILE=${1%.bayonet}.log

if [[ -z $1 ]]; then
  echo "usage: $0 <bayonet-file>"
  exit 1
fi

if [ ! -f $PSIFILE ]; then
    echo compiling $INPUT to $PSIFILE
    $BAYONET $INPUT &> $PSIFILE
else
  echo "skipping $1; $PSIFILE already exists."
fi

if [ ! -f $RESFILE ]; then
    echo "analyzing: $PSI $PSIFILE"
    { { time $PSI $PSIFILE; } 2>&1; } > $RESFILE
    tail $RESFILE
else
    echo "skipping $PSIFILE; $RESFILE already exists."
    tail $RESFILE
fi
