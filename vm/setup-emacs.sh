#!/usr/bin/env bash
set -ex

export OPAMYES=1
opam install ocp-index ocp-indent
git clone https://github.com/samoht/ocaml-emacs-settings.git
ln -s ocaml-emacs-settings/.emacs
ln -s ocaml-emacs-settings/.emacs.d/
