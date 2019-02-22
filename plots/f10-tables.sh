#!/bin/sh
echo "RESILIENCE ('true' means equivalent to teleport)"
for k in 0 1 2 3 4 inf ; do
    grep "resilience" examples/output/results/comparisons/$k-1-16.json | cut -d':' -f2 ;
done

echo "\nREFINEMENT ('true' means less than; 'false' means equivalent)"
for k in 0 1 2 3 4 inf ; do
    grep "refinement" examples/output/results/comparisons/$k-1-16.json | cut -d':' -f2 ;
done
