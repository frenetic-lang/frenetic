#!/bin/sh
rm ./common.pyc
echo "Intro..."
python ./plot_4cycle_intro.py ../data//4cycle_intro/
echo "Intro...link failure"
python ./plot_4cycle_intro.py ../data/4cycle_intro_break_1_2/
echo "Abilene..."
python ./plot_abilene.py ../data/abilene/
echo "Abilene... link failure"
python ./plot_abilene.py ../data/abilene_fail_0.1/
echo "Random walk... throughput convergence"
python ./plot_loop.py ../data/4cycle_loop/
echo "Heatmap..."
python ./plot_heatmap.py
echo "Done."
