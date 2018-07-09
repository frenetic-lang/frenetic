#!/usr/bin/env python3
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import re
import sys
from collections import defaultdict

# Regular expressions for parsing output
params_re = r"k = (?P<k>\d+) \| Pr\[failure\] = (?P<pr_failure>\S+) \| max failures = (?P<max_failures>\S+) \|\| timeout = (?P<timeout>\d+)"
bounds_re = r"bound = (?P<bound>\S+)"
file_re = r"(\S+)sw_(?P<num_switches>\d+)"
time_re = r"(?P<time>\d+(\.\d*)?) seconds"

def parse_output(output_file):
  # Parse output of probnetkat.bench
  results = []
  params_dict = None
  num_switches = None
  bound = None
  time = None
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
             results.append(result)
           else:
             print(l)
             raise Exception("Missing time information.")
         else:
           print(l)
  return results

def plot(data, logy=False):
  # constants
  max_failures = '-1'
  pr_failure = '1/16'
  bound_prefix = "Bound: "
  # lines: one line per 'bound', x-axis: num_switches, y-axis: time
  lines = defaultdict(dict)
  # Customize plots
  plt.figure(figsize=(6,4))
  ax = plt.subplot(111)    
  # ax.spines["top"].set_visible(False)
  # ax.spines["right"].set_visible(False)    
  ax.get_xaxis().tick_bottom()    
  ax.get_yaxis().tick_left() 

  max_x = 0
  max_y = 0
  # Plot data
  for pt in data:
    if pt['max_failures'] == max_failures and \
       pt['pr_failure'] == pr_failure and \
       (pt['bound'] <= 12 or pt['bound'] == float('inf')):
      lines[pt['bound']][pt['num_switches']] = pt['time']
  for bound, sw_times in sorted(lines.items()):
    sorted_pts = sorted(sw_times.items())
    xs, ys = zip(*sorted_pts)
    max_x = max(max_x, xs[-1])
    max_y = max(max_y, ys[-1])
    if logy:
      plt.semilogy(xs, ys, label = bound_prefix + str(bound))
    else:
      plt.plot(xs, ys, label = bound_prefix + str(bound))

  # Customize plots
  ax.grid(alpha=0.2)
  plt.xlim(0, max_x)
  plt.ylim(0.001, max_y)
  plt.xlabel("Number of switches")
  plt.ylabel("Compilation time (seconds)")
  # Legend order
  handles, labels = ax.get_legend_handles_labels()
  hl = sorted(zip(handles, labels), key=(lambda x : float(x[1][len(bound_prefix):])))
  handles2, labels2 = zip(*hl)
  ax.legend(handles2, labels2)
  plt.title("Compilation time vs. # switches (max_failures: " + max_failures + ", P[failure] = " + pr_failure + ")")
  if logy:
    plt.savefig("compilation-times-logy.pdf",  bbox_inches="tight")
  else:
    plt.savefig("compilation-times-linear.pdf",  bbox_inches="tight")

def main(data_file):
  data = parse_output(data_file)
  plot(data, True)
  plot(data, False)

if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: " + sys.argv[0] + " <output file of probnetkat.bench>")
    print ("\t`jbuilder exec probnetkat.bench 2>&1 | tee output_file`")
    sys.exit(1)
  else:
    main(sys.argv[1])
