#!/usr/bin/env python3
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
import re
import sys
from collections import defaultdict

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

# Regular expressions for parsing output
params_re = r"k = (?P<k>\d+) \| Pr\[failure\] = (?P<pr_failure>\S+) \| max failures = (?P<max_failures>\S+) \|\| timeout = (?P<timeout>\d+)"
bounds_re = r"bound = (?P<bound>\S+)"
cps_re = r"use_cps = (?P<use_cps>\S+)"
naive_dist_re = r"naive_dist = (?P<naive_dist>\S+)"
file_re = r"(\S+)sw_(?P<num_switches>\d+)"
time_re = r"(?P<time>\d+(\.\d*)?) seconds"

def get_cps_label(flag):
  if flag:
    return "CPS: Enabled"
  else:
    return "CPS: Disabled"

def get_naive_dist_label(flag):
  if flag:
    return " Naive dist."
  else:
    return ""


markers = {
  'Bound: 1' : 'D',
  'Bound: inf' : 's',
   get_cps_label(True) + get_naive_dist_label(True) : 's',
   get_cps_label(False) + get_naive_dist_label(True) : 'X',
   get_cps_label(True) + get_naive_dist_label(False) : 'o',
   get_cps_label(False) + get_naive_dist_label(False) : '+',
}

colors = {
  'Bound: 1' : 'firebrick',
  'Bound: inf' : 'darkgreen',
   get_cps_label(True) + get_naive_dist_label(True) : 'red',
   get_cps_label(False) + get_naive_dist_label(True) : 'blue',
   get_cps_label(True) + get_naive_dist_label(False) : 'darkgreen',
   get_cps_label(False) + get_naive_dist_label(False) : 'black',
}

def parse_output(output_file):
  # Parse output of probnetkat.bench
  results = []
  params_dict = None
  num_switches = None
  bound = None
  time = None
  use_cps = True  # True by default. False only if specified
  naive_dist = False  # False by default. True only if specified
  with open(output_file) as f:
   for line in f.readlines():
     l = re.sub('(?![ -~]).', ' ', line)
     params = re.match(params_re, l)
     if params is not None:
       params_dict = params.groupdict()
     else:
       bounds = re.match(bounds_re, l)
       if bounds is not None:
         bound_str = bounds.groupdict()['bound']
         if bound_str.isdigit():
           bound = int(bound_str)
         else:
           bound = float('inf')
       else:
         use_cps_match = re.match(cps_re, l)
         if use_cps_match is not None:
           use_cps_str = use_cps_match.groupdict()['use_cps']
           if 'true' in use_cps_str:
             use_cps = True 
           elif 'false' in use_cps_str:
             use_cps = False
         else:
           naive_dist_match = re.match(naive_dist_re, l)
           if naive_dist_match is not None:
             naive_dist_str = naive_dist_match.groupdict()['naive_dist']
             if 'true' in naive_dist_str:
               naive_dist = True 
             elif 'false' in naive_dist_str:
               naive_dist = False

           else:
             file_names = re.match(file_re, l)
             if file_names is not None:
               num_switches = file_names.groupdict()['num_switches']
               timestr = re.search(time_re, l)
               if timestr is not None:
                 time = timestr.groupdict()['time']
                 result = dict(params_dict)
                 result['num_switches'] = int(num_switches)
                 result['bound'] = bound 
                 result['time'] = float(time)
                 result['use_cps'] = use_cps 
                 result['naive_dist'] = naive_dist
                 use_cps = True  # Set it to true by default for next data point
                 naive_dist = False  # Set it to false by default for next data point
                 results.append(result)
               else:
                 print(l)
                 # raise Exception("Missing time information.")
             else:
               print(l)
  return results

def plot(data, plot_type):
  if plot_type == 0:  # Linear graph with bound=none, showing improvement with CPS, and non-naive-dist
    f = open("cps-gain.txt", "w")
    logy = False
    bounds = [float('inf')]
    cps_opts = [True, False]
    #naive_opts = [True, False]
    naive_opts = [False]
  elif plot_type == 1:  # Log-y graph comparing bound=none vs bound=1 with CPS=true & naive_dist=false
    f = open("star-overhead.txt", "w")
    logy = True
    bounds = [float('inf'), 1]
    cps_opts = [True]
    naive_opts = [False]
  else:
    raise Exception("Unknown plot type")

  # constants
  max_failures = '-1'
  pr_failure = '1/16'
  bound_prefix = 'Bound: '

  max_time = 2000

  # times: a list of times per 'bound', num_switches, use_cps, naive_dist. x-axis: num_switches, y-axis: time
  times = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(list))))
  time_mean = defaultdict(dict)
  time_std = defaultdict(dict)
 
  # Customize plots
  plt.figure(figsize=(4,2.5))
  ax = plt.subplot(111)    
  # ax.spines["top"].set_visible(False)
  # ax.spines["right"].set_visible(False)    
  ax.get_xaxis().tick_bottom()    
  ax.get_xaxis().set_ticks_position('both')
  ax.get_yaxis().tick_left() 
  ax.get_yaxis().set_ticks_position('both')
  ax.tick_params(axis='both', which='both', direction='in')
  if logy:
    ax.set_yscale("log", nonposy='clip')

  # Select data
  for pt in data:
    if pt['max_failures'] == max_failures and \
       pt['pr_failure'] == pr_failure and \
       pt['bound'] in bounds and \
       pt['use_cps'] in cps_opts and \
       pt['naive_dist'] in naive_opts:
      if pt['time'] < 3600:
        times[pt['bound']][pt['num_switches']][pt['use_cps']][pt['naive_dist']].append(pt['time'])

  # Compute mean and stddev
  for bound, sw_cps_nd_times in times.items():
    for sw, cps_nd_times in sw_cps_nd_times.items():
      for cps, nd_times in cps_nd_times.items():
        for naive_dist, time_vals in nd_times.items():
          label = None
          if plot_type == 0:
            label = get_cps_label(cps) + get_naive_dist_label(naive_dist)
            
          if plot_type == 1:
              label = bound_prefix + str(bound)
          time_mean[label][sw] = np.mean(time_vals)
          time_std[label][sw] = np.std(time_vals)

  # Plot data
  for label, sw_times in sorted(time_mean.items()):
    sorted_pts = sorted(sw_times.items())
    xs, ys = zip(*sorted_pts)
    errors = [time_std[label][x] for x in xs]
    plt.errorbar(xs, ys, yerr=errors, label = label,
                 marker=markers[label], color=colors[label], zorder=10)
    if label == get_cps_label(False) + get_naive_dist_label(False):
      plt.plot([xs[-1], 245], [ys[-1], max_time-100], ls=':',
                 marker=markers[label], color='gray', zorder=1)
      ax.text(245, max_time - 80, 'OOM', horizontalalignment='center', verticalalignment='top', color='red', bbox=dict(edgecolor='red', facecolor='white', alpha=0.8))

    # Also dump data to a file
    for idx in range(len(xs)):
      f.write(label + "\t" + str(xs[idx]) + "\t" + str(ys[idx]) + "\n") 
   


  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(0, 350)
  if logy:
    plt.ylim(0.001, max_time)
  else:
    plt.ylim(-20, max_time)
  # ax.fill_between([0,350], 3600, ax.get_ylim()[1], facecolor='red', alpha=0.2)
  plt.xlabel("Number of switches")
  plt.ylabel("Time (seconds)")
  leg = plt.legend(fancybox=True, loc='best')
  leg.get_frame().set_alpha(0.9)
 
  ax.spines['bottom'].set_color('#999999')
  ax.spines['top'].set_color('#999999') 
  ax.spines['right'].set_color('#999999')
  ax.spines['left'].set_color('#999999')

  # Legend order
  # handles, labels = ax.get_legend_handles_labels()
  # hl = sorted(zip(handles, labels), key=(lambda x : float(x[1][len(bound_prefix):])))
  # handles2, labels2 = zip(*hl)
  # ax.legend(handles2, labels2)
  
  # plt.title("Compilation time vs. # switches (max_failures: " + max_failures + ", P[failure] = " + pr_failure + ")")
  f.close()
  if logy:
    plt.savefig("f10-times-logy.pdf",  bbox_inches="tight")
  else:
    plt.savefig("f10-times-linear.pdf",  bbox_inches="tight")

def main(data_file):
  data = parse_output(data_file)
  plot(data, 0)
  plot(data, 1)

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: " + sys.argv[0] + " <output file of probnetkat.bench>")
    print ("\t`jbuilder exec probnetkat.bench 2>&1 | tee output_file`")
    sys.exit(1)
  else:
    main(sys.argv[1])
