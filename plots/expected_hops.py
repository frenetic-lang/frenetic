#!/usr/bin/env python3
import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import CommonConf
CommonConf.setupMPPDefaults()

prob_fails = ["1-128", "1-64", "1-32", "1-16", "1-8", "1-4"]
num_fail = "inf"

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

fps = []
fp_labels = []

for scene, specs in scenarios.items():
    lc = specs[0]
    ls = specs[1]
    ms = specs[2]
    exp_dir = specs[3]

    expected_hops = []
    fps = []
    fp_labels = []
    for prob_fail in prob_fails:
        file = num_fail + "-" + prob_fail + ".json"
        raw_data = Path(base_dir+exp_dir+file).read_text()
        data = json.loads(raw_data)
        hop_cdfs = data["hop_count_cdf"]
        hop_pdfs = [0] + [hop_cdfs[i+1] - hop_cdfs[i] for i in range(len(hop_cdfs)-1)]
        expected_hop_count = sum([(i+1) * hop_pdfs[i]/hop_cdfs[-1] for i in range(len(hop_pdfs))])
        expected_hops.append(expected_hop_count)
        fps.append(float(data["failure_prob"][0]) / data["failure_prob"][1])
        fp_labels.append(str(data["failure_prob"][0]) +"/" + str(data["failure_prob"][1]))
    plt.semilogx(fps,
             expected_hops,
             label=scene,
             linestyle = ls,
             color = lc,
             marker=ms)

#  plt.xlim(2, 14)
#  plt.ylim(0.6, 1)
plt.xlabel(r"Link failure probability")
plt.ylabel(r'E[hop count $|$ delivered]')
plt.legend(loc="upper left")
plt.xticks(fps, fp_labels)
plt.tight_layout()
plt.savefig("expected_hops_vs_fail_prob-"+num_fail+".pdf")
