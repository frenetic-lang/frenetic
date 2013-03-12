#!/bin/bash

echo "admit:"
find . -name "*.v" | xargs grep -c "admit\\." | grep -v ":0$"
echo "Admitted:"
find . -name "*.v" | xargs grep -c "Admitted\\." | grep -v ":0$"
echo "Axioms:"
find . -name "*.v" | xargs grep -c "Axiom" | grep -v ":0$"
