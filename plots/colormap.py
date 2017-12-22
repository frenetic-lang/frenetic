#!/usr/bin/env python3
import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors
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
    failures = math.inf if this['max_failures'] == -1 else this['max_failures']
    data[this['routing_scheme']][failures] = this['avg_prob_of_delivery']
  elif this['topology'] == topo and this['failure_prob'] == [0,1]:
    data[this['routing_scheme']][0] = this['avg_prob_of_delivery']


m = len(data.keys())
n = len(list(data.values())[0])
X = np.random.random((m,n))

xs = None
data = sorted(data.items(), key=lambda kv: sum(kv[1].values()))
for i, (scheme, probs) in enumerate(data):
  xs = sorted(probs.keys())
  ys = [probs[x] for x in xs]
  X[i,:] = ys


# plot 
fig, ax = plt.subplots()
im = plt.imshow(X, cmap=plt.get_cmap("Greys"))
fig.colorbar(im, orientation='vertical', label='Pr[delivery]')

plt.yticks(range(0, m), [x[0] for x in data])
xticks = [max(set(xs) - set([math.inf])) + 1 if x == math.inf else x for x in xs]
xtick_lbls = [' ∞ ' if x == math.inf else str(x) for x in xs]
plt.xticks(xticks, xtick_lbls)

plt.xlabel("max #failures")
plt.title("%s, Pr[failure] = %d/%d" % (topo, failure_prob[0], failure_prob[1]))

plt.show()
