#!/bin/bash

topos=("waxman,6,42")

pols=("bc")

ctrls=("" "-haskell" "-flowmod")

for topo in "${topos[@]}"
do
  for pol in "${pols[@]}"
  do
    for ctrl in "${ctrls[@]}"
    do
      cmd="../ocaml/VerifiedBenchmarks.native $ctrl -custom ../py/CustomTopo.py -topo $topo -policy $pol -autopcap -autolog"
      echo
      echo $cmd
      $cmd
      sudo mn -c 2> /dev/null
    done
  done
done
