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

methods = [
  'probnetkat_rn0',
  'probnetkat_rn0_fb0',
  'probnetkat_rn0_fb2',
  'prism.compiled',
  'prism.compiled_fb0',
]

label_of_method = {
  'probnetkat_rn0': 'PNK' ,
  'probnetkat_rn0_fb0': 'PNK (#f=0)',
  'probnetkat_rn0_fb2': 'PNK (#f≤2)',
  'prism.compiled': 'PRISM',
  'prism.compiled_fb0': 'PRISM (#f=0)',
}

markers = {
  'probnetkat_rn0' : 'o',
  'probnetkat_rn0_fb0' : '*',
  'probnetkat_rn0_fb2' : 'D',
  'prism.compiled' : 's',
  'prism.compiled_fb0': 'X',
  # 'probnetkat_rn0_fb2' : 'D',
  # 'prism_exact.compiled' : 'o',
  # 'probnetkat_rn0_fb2.compiled' : 'x',
}

colors = {
  'probnetkat_rn0' : 'darkgreen',
  'probnetkat_rn0_fb0' : 'orange',
  'probnetkat_rn0_fb2' : 'purple',
  'prism.compiled' : 'navy',
  'prism.compiled_fb0': 'red',
  # 'prism_exact.compiled' : 'green',
  # 'prism_approx.compiled' : 'black',
}

def parse_output(folder):
  results = []
  for file in glob(os.path.join(folder, '*.log*')):
    print (file)
    params = re.match(r'.*fat(?P<k>\d+)(?:(?P<fb>_fb-?\d))?\.(?P<method>\S+).log$', file)
    if params is None:
      raise Exception ("Could not parse file name: " + file)
    with open(file) as f:
      for l in f.readlines():
        realtime_line = re.match(r'real\t(?P<minutes>\d+)m(?P<seconds>\d+(\.\d*))s', l)
        if realtime_line is not None:
          minutes = int(realtime_line.groupdict()['minutes'])
          seconds = float(realtime_line.groupdict()['seconds'])
          result = dict(params.groupdict())
          result['time'] = minutes * 60 + seconds
          result['k'] = int(result['k'])
          result['num_switches'] = (5 * result['k']**2)/4
          if result.get('fb') == '_fb-1':
            result['fb'] = None
          if result['fb'] is not None:
            result['method'] += result['fb']
          results.append(result)
          # print(result)
          break
  return results
  

def plot(data):
  f = open("ecmp.txt", "w")

  times = defaultdict(lambda: defaultdict(list))
  time_mean = defaultdict(dict)
  time_std = defaultdict(dict)
  plt.figure(figsize=(6,3))
  ax = plt.subplot(111)    
  ax.get_xaxis().tick_bottom()    
  ax.get_xaxis().set_ticks_position('both')
  ax.get_yaxis().tick_left() 
  ax.get_yaxis().set_ticks_position('both')
  ax.tick_params(axis='both', which='both', direction='in')
  ax.set_xscale("log", nonposx='clip')
  ax.set_yscale("log", nonposy='clip')
  for pt in data:
    times[pt['method']][pt['num_switches']].append(pt['time'])
  for method, sw_times in times.items():
    if method in methods:
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
    plt.errorbar(xs, ys, yerr=errors, label = label_of_method[method],
                 marker=markers[method], color=colors[method], zorder=10)

    # Also dump data to a file
    for idx in range(len(xs)):
      f.write(method + "\t" + str(xs[idx]) + "\t" + str(ys[idx]) + "\n") 
   
  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(40, 6000)
  plt.ylim(2, 1100)
  ax.fill_between([0, 500], 3600, ax.get_ylim()[1], facecolor='red', alpha=0.2)
  ax.spines['bottom'].set_color('#999999')
  ax.spines['top'].set_color('#999999') 
  ax.spines['right'].set_color('#999999')
  ax.spines['left'].set_color('#999999')

  plt.xlabel("Number of switches")
  plt.ylabel("Time (seconds)")
  leg = plt.legend(fancybox=True, bbox_to_anchor=(1, 1))
  leg.get_frame().set_alpha(0.9)
  f.close()
  plt.savefig('ecmp.pdf', bbox_inches='tight')


def main():
  data = parse_output(DATA_DIR)
  plot(data)

if __name__ == "__main__":
    main()
