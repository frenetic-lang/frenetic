#!/usr/bin/env bash
set -ex

# install java (required by PRISM)
apt-get install -y software-properties-common python-software-properties
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
add-apt-repository -y -u ppa:webupd8team/java
apt-get install -y oracle-java8-installer
apt-get install oracle-java8-set-default

# install PRISM, Bayonet
cd frenetic/examples/bayonet && source install_dependencies.sh
