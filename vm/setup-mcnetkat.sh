#!/usr/bin/env bash
set -ex

# install latest version of ocaml
git clone https://github.com/frenetic-lang/frenetic.git
cd frenetic
git checkout mc-decision
opam pin add frenetic lib -y --no-action --working-dir --inplace-build
opam pin add probnetkat . -y --no-action --working-dir --inplace-build
opam install probnetkat -y