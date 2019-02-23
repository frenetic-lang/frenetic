#!/usr/bin/env bash
set -ex

# install mcnetkat
git clone https://github.com/frenetic-lang/frenetic.git
cd frenetic
git checkout mc-decision
opam pin add frenetic lib -y --working-dir --inplace-build --no-action
opam pin add probnetkat . -y --working-dir --inplace-build
opam install probnetkat -y
