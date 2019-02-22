#!/usr/bin/env python3
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import os
import re
import sys
from collections import defaultdict
from glob import glob

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

DATA_DIR = "."
SAMPLE = "sample.txt"

methods = [
  'probnetkat',
  # 'probnetkat_fb0',
  # 'probnetkat_fb2',
  'prism.compiled',
  # 'prism.compiled_fb0',
]

label_of_method = {
  'probnetkat': 'PNK' ,
  # 'probnetkat_fb0': 'PNK (#f=0)',
  # 'probnetkat_fb2': 'PNK (#f≤2)',
  'prism.compiled': 'PRISM',
  # 'prism.compiled_fb0': 'PRISM (#f=0)',
}

markers = {
  'probnetkat' : 'o',
  # 'probnetkat_fb0' : '*',
  # 'probnetkat_fb2' : 'D',
  'prism.compiled' : 's',
  # 'prism.compiled_fb0': 'X',
  # 'probnetkat_fb2' : 'D',
  # 'prism_exact.compiled' : 'o',
  # 'probnetkat_fb2.compiled' : 'x',
}

colors = {
  'probnetkat' : 'purple',
  # 'probnetkat_fb0' : 'orange',
  # 'probnetkat_fb2' : 'purple',
  'prism.compiled' : 'orange',
  # 'prism.compiled_fb0': 'red',
  # 'prism_exact.compiled' : 'green',
  # 'prism_approx.compiled' : 'black',
}

def parse_output(folder):
  # read in topology sizes
  size = dict()
  if not os.path.isfile(SAMPLE):
    raise Error("can't find '%s' - may need to run 'sample.py'?" % SAMPLE)
  with open(SAMPLE, 'r') as sample:
    for line in sample:
      n, m, topo = line.split()
      topo = os.path.basename(topo)
      topo = os.path.splitext(topo)[0]
      size[topo] = (int(n),int(m))

  results = []
  for file in glob(os.path.join(folder, '*.log')):
    params = re.match(r'^[^\w]*(?P<topo>[^.]+)\.(?P<method>.+)\.log$', file)
    if params is None:
      raise Exception ("Could not parse file name: " + file)
    result = dict(params.groupdict())
    if result['topo'] not in size:
      # this topo does not belong to our sample
      continue
    print(file)
    with open(file) as f:
      for l in f.readlines():
        realtime_line = re.match(r'real\t(?P<minutes>\d+)m(?P<seconds>\d+(\.\d*))s', l)
        if realtime_line is not None:
          minutes = int(realtime_line.groupdict()['minutes'])
          seconds = float(realtime_line.groupdict()['seconds'])
          result['time'] = minutes * 60 + seconds
          result['num_switches'] = size[result['topo']][0]
          result['num_edges'] = size[result['topo']][1]
          results.append(result)
          break
  return results
  

def lookup_prism(topo, data):
  prism = [ d['time'] for d in data if d['topo'] == topo and d['method'] == 'prism.compiled']
  print (topo + str(prism))
  if len(prism) > 0:
    return prism[0]
  else:
    return -1

def plot(data):
  plt.figure()
  pnk = [ (d['num_switches'], d['topo'], d['time']) for d in data if d['method']=="probnetkat"]
  pnk.sort()
  pnk_x, pnk_z, pnk_y = zip(*pnk)
  prism = [ (d['num_switches'], d['topo'], d['time']) for d in data if d['method']=="prism.compiled"]
  prism.sort()
  prism_x, prism_z, prism_y = zip(*prism)
  plt.scatter(pnk_x,pnk_y,c='r', marker='x', label="ProbNetKAT")
  plt.scatter(prism_x,prism_y,c='b', marker='.', label="Prism")
  ax = plt.gca()
  ax.tick_params(axis='both', which='both', direction='in')
  ax.set_xscale("log", nonposx='clip')
  ax.set_yscale("log", nonposy='clip')
  plt.xlabel("Number of switches")
  plt.ylabel("Time (seconds)")
  leg = plt.legend(fancybox=True)
  leg.get_frame().set_alpha(0.9)

  ax.grid(alpha=0.2)
  plt.xlim(1, 10000)
  plt.ylim(1, 150)
  plt.savefig('topozoo.pdf', bbox_inches='tight')

  # dump data to file
  with open("topozoo.txt", "w+") as f:
    for pnk_pt, prism_pt in zip(pnk, prism):
      f.write('{1:25} {method:10} {0:4} {2:8}\n'.format(*pnk_pt, method='NetKAT'))
      f.write('{1:25} {method:10} {0:4} {2:8}\n'.format(*prism_pt, method='Prism'))


def main():
  data = parse_output(DATA_DIR)
  # print(data)
  plot(data)

if __name__ == "__main__":
    main()
