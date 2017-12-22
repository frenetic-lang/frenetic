#!/usr/bin/env python3
import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
import os
from glob import glob

if len(argv) != 3:
  print("usage: %s [dir] [topo]" % argv[0])
  exit(1)

folder = argv[1]
topo = argv[2]
failure_prob = [0,1]
max_failures = -1

for file in glob(os.path.join(folder, '*/*.json')):
  print(file)
  raw_data = Path(file).read_text()
  this = json.loads(raw_data)
  if this['topology'] == topo and this['failure_prob'] == failure_prob and this['max_failures'] == max_failures:
    plt.plot(range(1,33), this['hop_count_cdf'], label=this['routing_scheme'])

plt.title("Pr[failure] = %d/%d, max failures = %s" % 
             (failure_prob[0], failure_prob[1], '∞' if max_failures == -1 else str(max_failures)))
plt.legend()
plt.xlabel("#hops")
plt.ylabel("Pr[#hops ≤ x]")
plt.show()
