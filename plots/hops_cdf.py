#!/usr/bin/env python3
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
base_dir = "../examples/output/results/"

scenarios = {
    "AB FatTree, F10 no rerouting": ["royalblue", "--", "^","abfattree_4_sw_20-f10_no_lr/"],
    "AB FatTree, F10 3-hop rerouting": ["darkgreen", "-.", "x", "abfattree_4_sw_20-f10_s1_lr/"],
    "AB FatTree, F10 3+5-hop rerouting": ["tomato", "-", ".", "abfattree_4_sw_20-f10_s1_s2_lr/"],
    "FatTree, F10 3+5-hop rerouting": ["black", ":", "P", "fattree_4_sw_20-f10_s1_s2_lr/"]
}

ax = plt.figure(figsize=(9,6)).gca()
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

plt.xlim(2, 14)
plt.ylim(0.6, 1)
plt.xlabel(r"Hop count")
plt.ylabel(r'Pr[hop~count $\leq x$]')
plt.legend(loc="lower right")
plt.tight_layout()
plt.savefig("cdf_tput_vs_hop_count-"+num_fail+"-"+prob_fail+".pdf")
