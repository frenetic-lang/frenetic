#!/usr/bin/env bash

apt-get update
apt-get install -y python-software-properties  build-essential m4
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install -y ocaml opam python-networkx wireshark
