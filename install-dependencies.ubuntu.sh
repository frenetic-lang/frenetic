#!/bin/bash
sudo apt update
sudo apt install python-dev graphviz libgraphviz-dev pkg-config
sudo apt install libsuitesparse-dev
sudo apt install python3-pip
sudo apt install libcurl4-nss-dev
sudo apt install swig

# python 3, to solve linear systems
sudo -H python3 -m pip install --upgrade pip
python3 -m pip list --outdated --format=freeze |\
  grep -v '^\-e' |\
  cut -d = -f 1  |\
  xargs -n1 sudo -H python3 -m pip install --user --upgrade
python3 -m pip install --user --upgrade \
  numpy scipy matplotlib ipython jupyter pandas sympy nose \
  scikit-umfpack cython networkx pygraphviz bson
