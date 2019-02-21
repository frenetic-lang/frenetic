#!/bin/bash

DIR=`dirname "$(readlink -f "$0")"`
echo $DIR
python3 ${DIR}/hops_cdf.py
python3 ${DIR}/expected_hops.py
python3 ${DIR}/tput_vs_fp.py
