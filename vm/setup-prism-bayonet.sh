#!/usr/bin/env bash
set -ex

# install java (required by PRISM)
sudo apt-get install -y software-properties-common python-software-properties
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo add-apt-repository -y -u ppa:webupd8team/java
sudo apt-get install -y oracle-java8-installer
sudo apt-get install oracle-java8-set-default

# install PRISM, Bayonet
cd frenetic/examples/bayonet && source install_dependencies.sh

# install python dependencies required to generate input files for Bayonet experiment
python3 -m pip install --user --upgrade networkx pygraphviz
sudo apt-get install -y python3-tk
