#!/bin/bash

for JOINED in $@
do
  sudo tcpdump -r $JOINED -w $JOINED.icmp "icmp"
  sudo tcpdump -r $JOINED -w $JOINED.of "tcp port 6633"
done
