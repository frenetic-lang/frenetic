#!/usr/bin/env python3

import os
import re
from glob import glob


def main():
   for old in glob("*.dot"):
      d,file = os.path.split(old)
      base, ext = os.path.splitext(file)
      match = re.match(r'^fattree_(\d+)$', base)
      if match is None:
         print("no match for %s with base %s" % (old, base))
         continue
      k = int(match.group(1))
      print("matched %s with k = %d" % (old, k))
      sw = (5 * k**2)/4
      base += "_sw_%d" % sw
      new = os.path.join(d, base) + ext
      print("-> new name: %s" % new)
      os.rename(old, new)

   for old in glob("*-allsp.nexthops"):
      d,file = os.path.split(old)
      base, ext = os.path.splitext(file)
      match = re.match(r'^fattree_(\d+)-allsp$', base)
      if match is None:
         print("no match for %s with base %s" % (old, base))
         continue
      k = int(match.group(1))
      print("matched %s with k = %d" % (old, k))
      sw = (5 * k**2)/4
      base = "fattree_%d_sw_%d-allsp" % (k, sw)
      new = os.path.join(d, base) + ext
      print("-> new name: %s" % new)
      os.rename(old, new)

   for old in glob("*-spf.trees"):
      d,file = os.path.split(old)
      base, ext = os.path.splitext(file)
      match = re.match(r'^fattree_(\d+)-spf$', base)
      if match is None:
         print("no match for %s with base %s" % (old, base))
         continue
      k = int(match.group(1))
      print("matched %s with k = %d" % (old, k))
      sw = (5 * k**2)/4
      base = "fattree_%d_sw_%d-spf" % (k, sw)
      new = os.path.join(d, base) + ext
      print("-> new name: %s" % new)
      os.rename(old, new)


if __name__ == '__main__':
   main()