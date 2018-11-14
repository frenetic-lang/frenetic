#!/bin/bash
sudo pkill -f RPC_PARALLEL_WORKER
sudo pkill -f probnetkat
for I in {1..24}
do
  ssh -t "abilene@atlas-$I" "sudo pkill -f RPC_PARALLEL_WORKER"
  ssh -t "abilene@atlas-$I" "sudo pkill -f probnetkat"
done
