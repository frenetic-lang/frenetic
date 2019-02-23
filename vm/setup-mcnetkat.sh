#!/usr/bin/env bash
set -ex

# install mcnetkat
if [ ! -d "frenetic" ]; then
  git clone -b mc-decision https://github.com/frenetic-lang/frenetic.git
  cd frenetic
else
  cd frenetic
  git fetch --all
  git checkout mc-decision
  git reset --hard origin/mc-decision
fi
opam pin add frenetic lib -y --working-dir --inplace-build --no-action
opam pin add probnetkat . -y --working-dir --inplace-build
opam reinstall probnetkat -y
