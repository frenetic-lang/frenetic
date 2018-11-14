#!/bin/bash
#
# Installs and starts memlockd on workers to avoid DOS.
#

for I in {24..1}
do
  ssh -t "abilene@atlas-$I" "sudo apt install memlockd && memlockd"
done
