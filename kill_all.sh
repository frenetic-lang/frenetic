#!/bin/bash
#
# Kills all worker processes, locally and remotely.
#

sudo pkill -f RPC_PARALLEL_WORKER
sudo pkill -f probnetkat
sudo pkill -f java  # PRISM

for I in {24..1}
do
  ssh -t "abilene@atlas-$I" << EOM
sudo pkill -f RPC_PARALLEL_WORKER
sudo pkill -f probnetkat
sudo pkill -f java
EOM
done
