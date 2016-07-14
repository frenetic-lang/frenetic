#!/bin/bash
for pol in `cat dot-files`; do
  wget http://storage.googleapis.com/compilekat/pols/sdx-netkat/$pol
done
