#!/bin/bash

FRENETIC=/home/jcollard/git/frenetic/frenetic/frenetic.native

$FRENETIC http-controller &
FPID=$!
sleep 1

python $@ &
PPID=$!
sleep 1

mn --controller=remote  --topo=single,2 --mac --arp
sleep 1

ps -auwwx | grep "python $@" |awk '{print $2}' | xargs kill
kill $PPID
kill $FPID
