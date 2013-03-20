#!/bin/bash

echo "admit:"
find . -name "*.v" | xargs grep -c "admit\\." | grep -v ":0$"
echo "Admitted:"
find . -name "*.v" | xargs grep -c "Admitted\\." | grep -v ":0$"
echo "Axioms:"
find . -name "*.v" | xargs grep -c "Axiom" | grep -v ":0$"
echo "Total admits":
find . -name "*.v" | xargs cat | grep -c "admit\\."
echo "Total Admitted:"
find . -name "*.v" | xargs cat | grep -c "Admitted\\."
echo "Total Axioms:"
find . -name "*.v" | xargs cat | grep -c "Axiom"
