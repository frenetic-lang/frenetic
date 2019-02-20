#!/usr/bin/env bash
set -ex

sudo apt-get update
sudo apt-get install -y python-dev graphviz libgraphviz-dev pkg-config
sudo apt-get install -y libsuitesparse-dev libgmp-dev
sudo apt-get install -y python3-pip
sudo apt-get install -y libcurl4-nss-dev swig

# python 3, to solve linear systems using UMFPACK
sudo -H python3 -m pip install --upgrade pip
set +e # be generous about failing updates
python3 -m pip list --outdated --format=freeze |\
  grep -v '^\-e' |\
  cut -d = -f 1  |\
  xargs -n1 sudo -H python3 -m pip install --user --upgrade
set -e
python3 -m pip install --user --upgrade \
  numpy scipy scikit-umfpack cython
