#!/bin/bash

EX_DIR="examples/coronet"
FSH="./fresh.native"

CURRENT="results-60.current"
COLLECTED="results-60.collected"
DISPLAY=".results-60.txt"

# Choose the synthesis backend according to the argument
if [ -z "$1" ]
then
    echo "No synthesis backend specified. Using existing backends specified in scripts."
    echo "Available backends are optical, lpe and sate."
else
    echo "Altering Coronet-60 scripts to use $1 backend."
    # Doesn't keep backups of the original script since the changes are tiny
    sed -i -- 's/\(coronet synthesize \)\([a-zA-Z]*\)/\1'$1'/' \
        examples/coronet/coronet-60-*.ash
fi

# Run the actual Frenetic shell scripts to perform the experiments. Save them to
# the current experimental file.
rm -f $CURRENT
for i in {1..10}
do
    nodes=$(($i * 2))
    echo "Running script" $EX_DIR/coronet-60-$i.ash "with $nodes edge nodes."
    cat $EX_DIR/coronet-60-$i.ash | $FSH >> $CURRENT
done

# Extract relevant information for display purposes
echo "EdgeNodes Policy Fabric Formulation Solution Generation" > $DISPLAY
cat $CURRENT | grep "\*\*\*" | tr -d "***" >> $DISPLAY

# Collect and save the results of this run
echo >> $COLLECTED
echo "===" >> $COLLECTED
echo "Date: " $(date "+%Y-%m-%d %T %Z") >> $COLLECTED
echo "Machine: " $(hostname) >> $COLLECTED
echo "Engine: " $1 >> $COLLECTED
column -t $DISPLAY >> $COLLECTED
echo "===" >> $COLLECTED

# Display the results of this run only
echo
echo "All times in nanoseconds"
column -t $DISPLAY
