#!/bin/bash
ARCHIVE="v4.4.zip"
PRISM_ROOT="prism-4.4/prism"

wget "https://github.com/prismmodelchecker/prism/archive/$ARCHIVE"
unzip $ARCHIVE
(cd $PRISM_ROOT && make)
mkdir -p $HOME/bin
ln -s $(realpath ${PRISM_ROOT}/bin/prism) $HOME/bin/prism
