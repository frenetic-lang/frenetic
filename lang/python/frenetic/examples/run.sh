#!/bin/bash

FRENETIC=/home/jcollard/git/frenetic/frenetic/frenetic.native

$FRENETIC http-controller &
FPID=$!
sleep 1

python $@ &
PPID=$!
sleep 1

mn --controller=remote  --topo=single,3 --mac --arp
sleep 1

kill $PPID
kill $FPID
