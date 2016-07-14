#!/bin/bash
TRIALS=`seq 1 10`
POLICIES=`ls *.json`

parallel --progress --slf $1 "cd $PWD && $HOME/compilekat/compilekat compile sdx {1} {2}" \
  ::: $POLICIES \
  ::: $TRIALS  | tr - ,
