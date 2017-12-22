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

for file in glob(os.path.join(folder, '*/*.json')):
  print(file)
  raw_data = Path(file).read_text()
  this = json.loads(raw_data)
  if this['topology'] == topo and this['failure_prob'] == [1,32] and this['max_failures'] == -1:
    plt.plot(range(1,33), this['hop_count_cdf'], label=this['routing_scheme'])


plt.legend()
plt.xlabel("#hops")
plt.ylabel("Pr[#hops ≤ x]")
plt.show()
