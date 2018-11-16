#!/usr/bin/env python3

import os
import subprocess

tools = {
  'probnetkat': {
    'cmd': lambda k:
      ['./run_probnetkat.sh', str(k)],
    'envs': [
      { 'RN': '0' },
      # { 'RN': '24' },
    ],
  },
  'prism.compiled': {
    'cmd': lambda k:
      ['./run_prism.compiled.sh', str(k)],
    'envs': [
      { },
    ],
  },
}


def main():
  errored = { tool : [False for _ in range(len(data['envs']))] 
                    for (tool, data) in tools.items()}
  for i in range(20):
    k = 2*(i+2)
    print("k = %d" % k)
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
        run = subprocess.run(data['cmd'](k),
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
