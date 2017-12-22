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
  if this['topology'] == topo and this['failure_prob'] == failure_prob and this['max_failures'] != -1:
    data[this['routing_scheme']][this['max_failures']] = this
  elif this['topology'] == topo and this['failure_prob'] == [0,1]:
    data[this['routing_scheme']][0] = this


m = len(data.keys())
n = len(list(data.values())[0])
X = np.random.random((m,n))


data = sorted(data.items(), key=lambda kv: kv[0])
for i, (scheme, probs) in enumerate(data):
  xs = sorted(probs.keys())
  ys = [probs[x]['avg_prob_of_delivery'] for x in xs]
  X[i,:] = ys
#   rows.append((scheme, ys))

plt.imshow(X)

plt.yticks(range(0, m), [x[0] for x in data])

# xticks = range(0,max_x+2)
# xtick_lbls = [str(t) for t in range(0,max_x+1)] + [' ∞ ']
# plt.xticks(xticks, xtick_lbls)


# plt.title("%s, Pr[failure] = %d/%d" % (topo, failure_prob[0], failure_prob[1]))
# plt.legend()
# plt.xlabel("max #failures")
plt.show()
