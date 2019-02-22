#!/usr/bin/env bash
set -ex

DIR=`dirname "$(readlink -f "$0")"`
# install java (required by PRISM)
sudo apt-get install -y software-properties-common python-software-properties
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo add-apt-repository -y -u ppa:webupd8team/java
sudo apt-get install -y oracle-java8-installer
sudo apt-get install oracle-java8-set-default

# install PRISM, Bayonet
pushd ${DIR}/../examples/bayonet
bash install_dependencies.sh
popd
