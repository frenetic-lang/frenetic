#!/bin/bash
#
# Installs and starts memlockd on workers to avoid DOS.
#
make && make install
for I in {24..1}
do
  ssh -t "abilene@atlas-$I" << EOM
sudo apt install memlockd
memlockd
if grep -Fxy "ulimit" ~/.bash_profile; then
  # ulimit already set
else
  echo -e "\n# increase stack size\nulimit -S -s 131072\n" >> ~/.bash_profile
fi
EOM
  scp $(which probnetkat.rpc_compile_branch) abilene@atlas-$I:/tmp/
done
