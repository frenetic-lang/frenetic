#!/bin/bash

run=1
while :
do
    echo "Starting run" $run
    ./coronet-$1.sh $2 $3
    echo "Sleeping for 30 seconds"
    sleep 3
    run=$(($run + 1))
    echo
done
