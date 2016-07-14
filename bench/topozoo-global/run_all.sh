#!/bin/bash

# start with fresh log file
rm -f log.txt
touch log.txt

for f in *.json
do
  ./run.sh $f | tee -a log.txt
done

echo
python3 summarize.py log.txt
