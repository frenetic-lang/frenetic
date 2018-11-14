#!/bin/bash
#
# Kills all worker processes, locally and remotely.
#

sudo pkill -f RPC_PARALLEL_WORKER
sudo pkill -f probnetkat

for I in {24..1}
do
  ssh -t "abilene@atlas-$I" "sudo pkill -f RPC_PARALLEL_WORKER"
  ssh -t "abilene@atlas-$I" "sudo pkill -f probnetkat"
done
