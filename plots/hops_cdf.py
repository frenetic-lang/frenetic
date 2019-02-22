#!/usr/bin/env python3
import matplotlib
matplotlib.use("Agg")

import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import CommonConf
CommonConf.setupMPPDefaults()

prob_fail = "1-4"
num_fail = "inf"

file = num_fail + "-" + prob_fail + ".json"
base_dir = "examples/output/results/"

scenarios = {
    "AB FatTree, $F10_0$": ["royalblue", "--", "^","abfattree_4_sw_20-f10_no_lr/"],
    "AB FatTree, $F10_ 3$": ["darkgreen", "-.", "x", "abfattree_4_sw_20-f10_s1_lr/"],
    "AB FatTree, $F10_{3,5}$": ["tomato", "-", ".", "abfattree_4_sw_20-f10_s1_s2_lr/"],
    "FatTree, $F10_{3,5}$": ["black", ":", "P", "fattree_4_sw_20-f10_s1_s2_lr/"]
}

ax = plt.figure(figsize=(3,3)).gca()
ax.xaxis.set_major_locator(MaxNLocator(integer=True))
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)

for scene, specs in scenarios.items():
    lc = specs[0]
    ls = specs[1]
    ms = specs[2]
    exp_dir = specs[3]
    raw_data = Path(base_dir+exp_dir+file).read_text()
    data = json.loads(raw_data)
    plt.plot(range(1, len(data["hop_count_cdf"])+1),
             data["hop_count_cdf"],
             label=scene,
             linestyle = ls,
             color = lc,
             marker=ms,
             drawstyle='steps-post')

plt.xlim(2, 15)
plt.ylim(0.6, 1)
plt.xlabel("Hop count")
plt.ylabel('Pr[hop count $\leq x$]')
plt.legend(loc="lower right")
plt.tight_layout()
plt.savefig("experiments/f10/cdf_tput_vs_hop_count-"+num_fail+"-"+prob_fail+".pdf")
