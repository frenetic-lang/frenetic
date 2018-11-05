#!/bin/bash
ARCHIVE="v4.4.zip"
PRISM_ROOT="prism-4.4/prism"

wget "https://github.com/prismmodelchecker/prism/archive/$ARCHIVE"
unzip $ARCHIVE
cd $PRISM_ROOT && make
