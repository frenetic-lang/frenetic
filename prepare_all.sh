#!/bin/bash
#
# Installs and starts memlockd on workers to avoid DOS.
#
make && make install
for I in {24..1}; do
  ssh "abilene@atlas-$I" /bin/bash << 'EOF'
sudo apt-get install memlockd
memlockd
if grep -q ulimit ~/.bash_profile; then
  true  # already set
else
  echo -e "\n# increase stack size\nulimit -S -s 131072\n" >> ~/.bash_profile
fi
EOF
  scp $(which probnetkat.rpc_compile_branch) abilene@atlas-$I:/tmp/
done
