#!/bin/bash

EX_DIR="examples/coronet"
FSH="./fresh.native"

# Choose the synthesis backend according to the argument
if [ -z "$1" ]
then
    echo "No synthesis backend specified. Using existing backends specified in scripts."
    echo "Available backends are optical, lpe and sate."
else
    echo "Altering Coronet-60 scripts to use $1 backend."
    # Doesn't keep backups of the original script since the changes are tiny
    sed -i '' 's/\(coronet synthesize \)\([a-zA-Z]*\)/\1'$1'/' \
        examples/coronet/coronet-60-*.ash
fi

# Run the actual Frenetic shell scripts to perform the experiments
for i in {1..10}
do
    nodes=$(($i * 2))
    echo "Running script" $EX_DIR/coronet-60-$i.ash "with $nodes edge nodes."
    cat $EX_DIR/coronet-60-$i.ash | $FSH >> results-60.txt
done

echo "EdgeNodes	Pre-processing	Formulation	Solution Generation" > .results-60.txt
cat results-60.txt | grep "\*\*\*" | tr -d "***" >> .results-60.txt

echo
echo "All times in nanoseconds"
column -t .results-60.txt
