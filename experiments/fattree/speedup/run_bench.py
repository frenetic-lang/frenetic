#!/usr/bin/env python3

import os
import subprocess

tools = {
  'probnetkat': {
    'cmd': lambda k:
      ['./run_probnetkat.sh', str(k)],
    'envs': [
      { 'J': '1', 'RJ':'0', 'RN': '0' },
      { 'J': '2', 'RJ':'0', 'RN': '0' },
      { 'J': '4', 'RJ':'0', 'RN': '0' },
      { 'J': '6', 'RJ':'0', 'RN': '0' },
      { 'J': '8', 'RJ':'0', 'RN': '0' },
      { 'J': '10', 'RJ':'0', 'RN': '0' },
      { 'J': '12', 'RJ':'0', 'RN': '0' },
      { 'J': '14', 'RJ':'0', 'RN': '0' },
      { 'J': '16', 'RJ':'0', 'RN': '0' },
      { 'J': '16', 'RJ':'2', 'RN': '1' },
      { 'J': '16', 'RJ':'6', 'RN': '1' },
      { 'J': '16', 'RJ':'10', 'RN': '1' },
      { 'J': '16', 'RJ':'9', 'RN': '2' },
      { 'J': '16', 'RJ':'12', 'RN': '2' },
      { 'J': '16', 'RJ':'15', 'RN': '2' },
      { 'J': '16', 'RJ':'12', 'RN': '3' },
      { 'J': '16', 'RJ':'15', 'RN': '3' },
      { 'J': '16', 'RJ':'13', 'RN': '4' },
      { 'J': '16', 'RJ':'15', 'RN': '4' },
      { 'J': '16', 'RJ':'14', 'RN': '5' },
      { 'J': '16', 'RJ':'16', 'RN': '5' },
    ],
  },
}


def main():
  k = 14
  print("k = %d" % k)
  print("=" * 80)

  for tool, data in tools.items():
    print("\n%s" % tool.capitalize())
    print("-" * 80)
    # SJS: fastest first
    for i, env in enumerate(data['envs'][::-1]):
      my_env = os.environ.copy()
      my_env.update(env)
      print("running with environment %s..." % str(env))
      run = subprocess.run(data['cmd'](k),
                           env=my_env
                           )
      print("return code: %d" % run.returncode)
      if run.returncode != 0:
        print("-> errored.")
      else:
        print("-> ok.\n")
  

if __name__ == "__main__":
  main()
