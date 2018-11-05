#!/usr/bin/env python3

from subprocess import call


def bayonet(k):
  return ['./run_bayonet.sh', 'bayonet_resilience_sw_%d.bayonet' % 4*k]


def pnk(k):
  return ['./run_probnetkat.sh', 'bayonet_resilience_sw_%d.dot' % 4*k]


def prism(k):
  return ['./run_prism.sh', str(k)]


def run():
  bayonet = pnk = prism = True
  for i in range(1):
    k = 2**i
    print("k = %d" % k)
    print("=" * 80)

    if bayonet:
      bayonet = call(bayonet(k)) == 0
    
    if pnk:
      pnk = call(pnk(k)) == 0
    
    if prism:
      prism = call(prism(k)) == 0
    
    print("\n")
  

if __name__ == "__main__":
  run()
