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
from prettytable import PrettyTable


DATA_DIR = "."

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

# results will be sorted lexiographically by their parameters
PARAMS = [
  'scheme',
  'k',
  'prism',
  'cps',
  'dont_iterate',
  'parallelize',
]

methods = {
  'native (SPF)' : {
    'prism': 'False', 'dont_iterate': 'False', 'scheme': 'SPF',
  },
  # 'native (ECMP)' : {
  #   'prism': 'False', 'dont_iterate': 'False', 'scheme': 'ECMP',
  # },
  # 'native (RW)' : {
  #   'prism': 'False', 'dont_iterate': 'False', 'scheme': 'RW',
  # },
  'PRISM (SPF)' : {
    'prism': 'True', 'scheme': 'SPF',
  },
  # 'PRISM (ECMP)' : {
  #   'prism': 'True', 'scheme': 'ECMP',
  # },
}


markers = {
  'native (SPF)' : 'o',
  'native (ECMP)': 'x',
  'native (RW)': 'x',
  'PRISM (SPF)' : 's',
  'PRISM (ECMP)' : '+',
}

colors = {
  'native (SPF)' : 'navy',
  'native (ECMP)' : 'green',
  'native (RW)' : 'purple',
  'PRISM (SPF)' : 'orange',
  'PRISM (ECMP)' : 'black',
}

def parse_output(folder):
  results = []
  for file in glob(os.path.join(folder, '*.log')):
    print(file)
    result = dict()
    
    # parse parameters
    for param in PARAMS:
      match = re.search(r'%s=(?:(?P<n>\d+)|(?P<s>[a-zA-Z]+))' % param, file)
      if match is None:
        raise Exception("Could not parse file name: %s" % file)
      match = match.groupdict()
      if match['n'] is not None:
        result[param] = int(match['n'])
      else:
        result[param] = match['s']
    
    # parse result
    with open(file) as f:
      log = '\n'.join(f.readlines()[::-1])
      time = r'TIME: (?P<time>\d+(\.\d*))\.'
      timeout = r'TIMEOUT: (?P<timeout>\d+) seconds\.'
      error = r'ERROR: (?P<error>\d+)\.'
      match = re.search(r'(:?%s)|(:?%s)|(:?%s)' % (time, timeout, error), log)
      if match is None:
        raise Exception("Could not parse result in %s" % file)
      outcome = match.groupdict()
      if outcome['time'] is not None:
        result['time'] = float(outcome['time'])
      elif outcome['timeout'] is not None:
        result['timeout'] = float(outcome['timeout'])
      elif outcome['error'] is not None:
        result['error'] = int(outcome['error'])
      else:
        assert False
    # add to results
    # print(result)
    results.append(result)
  return results


def dump(data):
  t = PrettyTable()
  cols = PARAMS + ['RESULT']
  t.field_names = [c for c in cols]
  for point in data:
    row = [point[p] for p in PARAMS]
    if 'time' in point:
      row.append(point['time'])
    elif 'timeout' in point:
      row.append('TIMEOUT')
    elif 'error' in point:
      row.append('ERROR')
    t.add_row(row)
  for c in PARAMS:
    t.sortby = c
  with open('fattree.txt', 'w+') as f:
    f.write(str(t))
  

def plot(data):
  plt.figure(figsize=(6,3))
  ax = plt.subplot(111)    
  ax.get_xaxis().tick_bottom()    
  ax.get_xaxis().set_ticks_position('both')
  ax.get_yaxis().tick_left() 
  ax.get_yaxis().set_ticks_position('both')
  ax.tick_params(axis='both', which='both', direction='in')
  ax.set_xscale("log", nonposx='clip')
  ax.set_yscale("log", nonposy='clip')

  # only needs successful points
  data = [pt for pt in data if 'time' in pt]

  # label points with their method and num_switches
  for pt in data:
    pt['num_switches'] = (pt['k']**2 * 5)/4
    for method, filtr in methods.items():
      if all(pt[k] == v for k,v in filtr.items()):
        pt['method'] = method
        break
  data = [pt for pt in data if 'method' in pt]

  # compute times
  times = defaultdict(lambda: defaultdict(list))
  time_mean = defaultdict(dict)
  time_std = defaultdict(dict)
  for pt in data:
    times[pt['method']][pt['num_switches']].append(pt['time'])
  for method, sw_times in times.items():
    for sw, time_vals in sw_times.items():
      time_mean[method][sw] = np.mean(time_vals)
      time_std[method][sw] = np.std(time_vals)

  for method, sw_times in sorted(time_mean.items()):
    sorted_pts = sorted(sw_times.items())
    xs, ys = zip(*sorted_pts)
    if ys[-1] >= 3599:
      ys = ys[:-1]
      xs = xs[:-1]
    errors = [time_std[method][x] for x in xs]
    plt.errorbar(xs, ys, yerr=errors, label=method, marker=markers[method],
                 color=colors[method], zorder=10)
   
  # # ax.text(400, 500, 'Time limit = 3600s', horizontalalignment='center', verticalalignment='center', color='gray')
  # # ax.annotate("", xy=(400, 3600), xytext=(400, 1000), arrowprops=dict(arrowstyle="->", color='gray'))

  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(1, 100000)
  plt.ylim(0.5, 10000)
  ax.fill_between([0,100000], 3600, ax.get_ylim()[1], facecolor='red', alpha=0.2)
  ax.spines['bottom'].set_color('#999999')
  ax.spines['top'].set_color('#999999') 
  ax.spines['right'].set_color('#999999')
  ax.spines['left'].set_color('#999999')

  plt.xlabel("Number of switches")
  plt.ylabel("Time (seconds)")
  leg = plt.legend(fancybox=True, loc='best')
  leg.get_frame().set_alpha(0.9)
  plt.savefig('fattree.pdf', bbox_inches='tight')


def main():
  data = parse_output(DATA_DIR)
  dump(data)
  plot(data)
  # plot(data, methods)

if __name__ == "__main__":
  main()
