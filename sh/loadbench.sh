#!/bin/bash

topos=("wattsstrogatz,6" "wattsstrogatz,8" "wattsstrogatz,10" "fattree,4"  "fattree,6"  "fattree,8" "waxman")

pols=("sp" "bc")

ctrls=("" "-flowmod")

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