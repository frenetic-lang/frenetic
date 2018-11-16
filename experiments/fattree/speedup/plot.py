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
  'fattree14',
  'fattree16',
  # 'prism.compiled',
  # 'prism.compiled_fb0',
]

label_of_method = {
  'fattree14': '245 switches' ,
  'fattree16': '320 switches',
  # 'prism.compiled': 'PRISM',
  # 'prism.compiled_fb0': 'PRISM (#f=0)',
}

markers = {
  'fattree14' : 'o',
  'fattree16' : '*',
  # 'prism.compiled' : 's',
  # 'prism.compiled_fb0': 'X',
  # 'prism_approx' : 'D',
  # 'prism_exact.compiled' : 'o',
  # 'prism_approx.compiled' : 'x',
}

colors = {
  'fattree14' : 'darkgreen',
  'fattree16' : 'orange',
  # 'prism.compiled' : 'navy',
  # 'prism.compiled_fb0': 'red',
  # 'prism_approx' : 'purple',
  # 'prism_exact.compiled' : 'green',
  # 'prism_approx.compiled' : 'black',
}

def parse_output(folder):
  results = []
  for file in glob(os.path.join(folder, '*.log*')):
    print (file)
    params = re.match(r'.*fat(?P<k>\d+)\.[^_]*_j(?P<j>\d+)_rj(?P<rj>\d+)_rn(?P<rn>\d+)\.log$', file)
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
          for k in ['j', 'rj', 'rn']:
            result[k] = int(result[k])
          result['num_cores'] = result['j'] + result['rn'] * result['rj']
          result['method'] = 'fattree' + result['k']
          results.append(result)
          print(result)
          break
  return results
  

def plot(data):
  f = open("speedup.txt", "w")

  t0 = max(pt['time'] for pt in data)
  for pt in data:
    pt['speedup'] = t0/pt['time']

  speedups = defaultdict(dict)
  plt.figure(figsize=(5,3))
  ax = plt.subplot(111)    
  ax.get_xaxis().tick_bottom()    
  ax.get_xaxis().set_ticks_position('both')
  ax.get_yaxis().tick_left() 
  ax.get_yaxis().set_ticks_position('both')
  ax.tick_params(axis='both', which='both', direction='in')
  ax.set_xscale("linear", nonposx='clip')
  ax.set_yscale("linear", nonposy='clip')
  for pt in data:
    speedups[pt['method']][pt['num_cores']] = pt['speedup']

  for method, sw_speedups in sorted(speedups.items()):
    sorted_pts = sorted(sw_speedups.items())
    xs, ys = zip(*sorted_pts)
    plt.errorbar(xs, ys, label=label_of_method[method],
                 marker=markers[method], color=colors[method], zorder=10)

    # Also dump data to a file
    for idx in range(len(xs)):
      f.write(method + "\t" + str(xs[idx]) + "\t" + str(ys[idx]) + "\n") 
   
  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(0, 100)
  plt.ylim(0, 50)
  # identity function
  add_identity(ax, c='.3', ls='--')
  ax.spines['bottom'].set_color('#999999')
  ax.spines['top'].set_color('#999999') 
  ax.spines['right'].set_color('#999999')
  ax.spines['left'].set_color('#999999')

  plt.xlabel("Number of cores")
  plt.ylabel("Speedup")
  leg = plt.legend(fancybox=True, bbox_to_anchor=(1, 1))
  leg.get_frame().set_alpha(0.9)
  f.close()
  plt.savefig('speedup.pdf', bbox_inches='tight')


# https://stackoverflow.com/questions/22104256/does-matplotlib-have-a-function-for-drawing-diagonal-lines-in-axis-coordinates
def add_identity(axes, *line_args, **line_kwargs):
    identity, = axes.plot([], [], *line_args, **line_kwargs)
    def callback(axes):
        low_x, high_x = axes.get_xlim()
        low_y, high_y = axes.get_ylim()
        low = max(low_x, low_y)
        high = min(high_x, high_y)
        identity.set_data([low, high], [low, high])
    callback(axes)
    axes.callbacks.connect('xlim_changed', callback)
    axes.callbacks.connect('ylim_changed', callback)
    return axes


def main():
  data = parse_output(DATA_DIR)
  plot(data)

if __name__ == "__main__":
    main()
