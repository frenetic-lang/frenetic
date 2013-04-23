#!/bin/bash
echo "Controller,Experiment,Topology,Traffic"
for file in *.pcap.of
do
  capinfos -dTb $file | tail -n 1 | sed "s/-/ /g" | sed "s/\\.pcap\\.of//g"
done