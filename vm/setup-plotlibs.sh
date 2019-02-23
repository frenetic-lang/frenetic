#!/usr/bin/env bash
set -ex

# plotting libraries
sudo apt-get install -y xpdf # to view pdfs
python3 -m pip install --user --upgrade matplotlib brokenaxes
