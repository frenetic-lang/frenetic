#!/usr/bin/env python3

import os
import os.path
import subprocess

SAMPLE = 'sample.txt'

tools = {
  'probnetkat': {
    'cmd': lambda dotfile:
      ['./run_probnetkat.sh', dotfile],
    'envs': [
      { 'RN': '0' },
      # { 'RN': '24' },
    ],
  },
  'prism.compiled': {
    'cmd': lambda dotfile:
      ['./run_prism.compiled.sh', dotfile],
    'envs': [
      { },
    ],
  },
}


def main():
  if not os.path.isfile(SAMPLE):
    raise Error("can't find '%s' - may need to run 'sample.py'?" % SAMPLE)
  with open(SAMPLE, 'r') as sample:
    errored = { tool : [False for _ in range(len(data['envs']))] 
                      for (tool, data) in tools.items()}
    for line in sample:
      n, m, topo = line.split()
      print("%s (|V|=%s, |E|=%s)" % (topo, n, m))
      print("=" * 80)

      for tool, data in tools.items():
        if all(errored[tool]):
          continue
        print("\n%s" % tool.capitalize())
        print("-" * 80)
        for i, env in enumerate(data['envs']):
          if errored[tool][i]:
            continue
          my_env = os.environ.copy()
          my_env.update(env)
          print("running with environment %s..." % str(env))
          run = subprocess.run(data['cmd'](topo),
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE,
                               env=my_env
                               )
          print("return code: %d" % run.returncode)
          if run.returncode != 0:
            print("-> errored.")
            errored[tool][i] = True
          else:
            print("-> ok.\n")
      
      print("\n\n")
  

if __name__ == "__main__":
  main()
