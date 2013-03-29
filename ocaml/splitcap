#!/bin/bash

JOINED=$1
sudo tcpdump -r $JOINED -w $JOINED.icmp "icmp"
sudo tcpdump -r $JOINED -w $JOINED.of "tcp port 6633"
capinfos -dT $JOINED.icmp
capinfos -dT $JOINED.of