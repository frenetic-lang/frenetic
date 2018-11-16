#!/usr/bin/env python3

import networkx as nx
import pydot
import glob
import random


def sample(list, n):
   return [ list[i] for i in sorted(random.sample(range(len(list)), n)) ]

def size(topo):
   G = nx.drawing.nx_pydot.read_dot(topo)
   return (len(G.nodes()), len(G.edges()))


def main():
   topos = list(glob.glob("../../examples/topozoo/*.dot"))
   # topos = [(len(nx.drawing.nx_pydot.read_dot(topo)), topo)
   #           for topo in glob.glob("../../examples/topozoo/*.dot")]
   # topos.sort(key=lambda x: x[0])
   # print(len(topos))
   samples = sample(topos, 30)
   d = { t : size(t) for t in samples }
   print('\n'.join(["%d\t%d\t%s" % (d[t][0], d[t][1], t) for t in d]))


if __name__ == "__main__":
   main()
