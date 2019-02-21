#!/usr/bin/env bash
set -ex

# install java (required by PRISM)
add-apt-repository ppa:webupd8team/java
apt-get update
apt-get install -y oracle-java8-installer

# install PRISM, Bayonet
cd frenetic/examples/bayonet && source install_dependencies.sh
