#!/usr/bin/env python3

import os
import re
from glob import glob


def main():
   for dot in glob("*.dot"):
      d,file = os.path.split(dot)
      base, ext = os.path.splitext(file)
      match = re.match(r'^fattree_(\d+)$', base)
      if match is None:
         print("no match for %s with base %s" % (dot, base))
         continue
      k = int(match.group(1))
      print("matched %s with k = %d" % (dot, k))
      sw = (5 * k**2)/4
      base += "_sw_%d" % sw
      new = os.path.join(d, base) + ext
      print("-> new name: %s" % new)
      os.rename(dot, new)


if __name__ == '__main__':
   main()