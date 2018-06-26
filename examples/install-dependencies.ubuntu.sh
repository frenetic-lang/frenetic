#!/bin/bash

sudo apt-add-repository -y ppa:aims/sagemath
sudo apt update
sudo apt install \
  sagemath-upstream-binary python-dev graphviz libgraphviz-dev pkg-config

sudo sage -pip install --upgrade pip
sudo sage -pip install netaddr pygraphviz
