#!/usr/bin/env bash
set -ex

# install latest version of ocaml
opam switch create 4.07.1 -y
eval $(opam config env)