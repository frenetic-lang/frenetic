#!/bin/bash
set -e

sizes=( 100 200 400 800 1000 2000 4000 8000 16000 32000 )
for seed in `cd seeds && ls`; do
  src="seeds/$seed" 
  for size in "${sizes[@]}"; do
    dst=$seed-$size.json
    if [[ ! -e $dst ]]; then
      echo "Building $dst..."
      ./db_generator -bc $src $size 0 0 0 $src-$size.bench > /dev/null
      $BENCH classbench $src-$size.bench $dst
    fi
  done
done
