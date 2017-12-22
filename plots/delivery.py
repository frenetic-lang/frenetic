#!/usr/bin/env python3
import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
import os
from glob import glob
from collections import defaultdict
import math

if len(argv) != 3:
  print("usage: %s [dir] [topo]" % argv[0])
  exit(1)

folder = argv[1]
topo = argv[2]
failure_prob = [1,8]

data = defaultdict(dict)


for file in glob(os.path.join(folder, '*/*.json')):
  print(file)
  raw_data = Path(file).read_text()
  this = json.loads(raw_data)
  if this['topology'] == topo and this['failure_prob'] == failure_prob:
    data[this['routing_scheme']][this['max_failures']] = this
  elif this['topology'] == topo and this['failure_prob'] == [0,1]:
    data[this['routing_scheme']][0] = this

max_x = None

for scheme, probs in data.items():
  xs = sorted(probs.keys())
  ys = [probs[x]['avg_prob_of_delivery'] for x in xs]
  max_x = max(xs)

  # -1 encodes infinity
  if xs[0] == -1:
    xs[0] = max_x + 1
    xs = np.roll(xs, -1)
    ys = np.roll(ys, -1)

  plt.plot(xs, ys, 'x-',label=scheme)

ticks = range(0,max_x+2)
tick_lbls = [str(t) for t in range(0,max_x+1)] + [' ∞ ']
plt.xticks(ticks, tick_lbls)
plt.title("Pr[failure] = %d/%d" % (failure_prob[0], failure_prob[1]))
plt.legend()
plt.xlabel("max #failures")
plt.ylabel("Pr[delivery | #failures ≤ x]")
plt.show()
