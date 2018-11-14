#!/bin/bash
sudo pkill -f RPC_PARALLEL_WORKER
sudo pkill -f probnetkat
for I in 16 17 18 19 20 21 22 23 24
do
  ssh -t "abilene@atlas-$I" "sudo pkill -f RPC_PARALLEL_WORKER"
  ssh -t "abilene@atlas-$I" "sudo pkill -f probnetkat"
done