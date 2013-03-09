#! /bin/sh

set -e

# Bootstrap configure system from .ac/.am files
autoreconf --install --force
