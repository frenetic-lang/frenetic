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

label_of_method = {
  'bayonet' : 'Bayonet',
  'probnetkat' : 'ProbNetKAT',
  'probnetkat_no_cps' : 'ProbNetKAT (no CPS)',
  'prism' : 'Prism',
  'prism-approx' : 'Prism (approximate)',
}

markers = {
  'bayonet' : 'o',
  'probnetkat' : 's',
  'probnetkat_no_cps' : '*',
  'prism': 'X',
  'prism-approx' : 'D',
}

colors = {
  'bayonet' : 'darkgreen',
  'probnetkat' : 'navy',
  'probnetkat_no_cps' : 'orange',
  'prism' : 'red',
  'prism-approx' : 'purple',
}

def parse_output(folder):
  results = []
  for file in glob(os.path.join(folder, '*.log*')):
    print (file)
    params = re.match(r'.*bayonet.*sw_(?P<num_switches>\d+).(?P<method>\S+).log.*', file)
    if params is None:
      raise Exception ("Could not parse file name: " + file)
    with open(file) as f:
      for l in f.readlines():
         realtime_line = re.match(r'real\t(?P<minutes>\d+)m(?P<seconds>\d+(\.\d*))s', l)
         if realtime_line is not None:
           minutes = int(realtime_line.groupdict()['minutes'])
           seconds = float(realtime_line.groupdict()['seconds'])
           result = dict()
           result['time'] = minutes * 60 + seconds
           result['num_switches'] = int(params.groupdict()['num_switches'])
           result['method'] = params.groupdict()['method']
           results.append(result)
           break
  return results
  

def plot(data, methods):
  f = open("bayonet.txt", "w")

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
    errors = [time_std[method][x] for x in xs]
    plt.errorbar(xs[:-1], ys[:-1], yerr=errors[:-1], label = label_of_method[method],
                 marker=markers[method], color=colors[method], zorder=10)
    plt.errorbar(xs[-2:], ys[-2:], yerr=errors[-2:],
                 marker=markers[method], color='gray', ls=':', zorder=1)
    if method == 'bayonet':
      text = 'OOM after\ntiming out'
      ha = 'center'
    else:
      text = 'Timed out\nafter OOM'
      ha = 'right'
    ax.text(xs[-1], ys[-1], text, horizontalalignment=ha, verticalalignment='center', color=colors[method], bbox=dict(edgecolor='red', facecolor='white', alpha=0.8))

    # Also dump data to a file
    for idx in range(len(xs)):
      f.write(method + "\t" + str(xs[idx]) + "\t" + str(ys[idx]) + "\n") 
   
  ax.text(400, 500, 'Time limit = 3600s', horizontalalignment='center', verticalalignment='center', color='gray')
  ax.annotate("", xy=(400, 3600), xytext=(400, 1000), arrowprops=dict(arrowstyle="->", color='gray'))

  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(1, 10000)
  plt.ylim(0.5, 10000)
  ax.fill_between([0,10000], 3600, ax.get_ylim()[1], facecolor='red', alpha=0.2)
  ax.spines['bottom'].set_color('#999999')
  ax.spines['top'].set_color('#999999') 
  ax.spines['right'].set_color('#999999')
  ax.spines['left'].set_color('#999999')

  plt.xlabel("Number of switches")
  plt.ylabel("Time (seconds)")
  leg = plt.legend(fancybox=True, loc='best')
  leg.get_frame().set_alpha(0.9)
  f.close()
  plt.savefig('bayonet.pdf', bbox_inches='tight')


def main(data_dir):
  data = parse_output(data_dir)
  plot(data, ['bayonet', 'probnetkat', 'probnetkat_no_cps', 'prism', 'prism-approx'])

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: " + sys.argv[0] + " <output directory with logs>")
    sys.exit(1)
  else:
    main(sys.argv[1])
