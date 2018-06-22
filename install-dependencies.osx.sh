#!/bin/bash
brew install suite-sparse graphviz

# python 3, to solve linear systems
sudo -H python3 -m pip install --upgrade pip
sudo -H python3 -m pip install \
  $(pip3 list --outdated --format=legacy | awk '{ print $1 }') --upgrade
python3 -m pip install --user --upgrade \
  numpy scipy matplotlib ipython jupyter pandas sympy nose scikit-umfpack bson

# python 2, for generating topologies
sudo -H python -m pip install --upgrade pip
python -m pip install --user --upgrade \
  networkx matplotlib netaddr pygraphviz