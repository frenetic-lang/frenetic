#!/usr/bin/env python3

import os
import subprocess

tools = {
  'bayonet': {
    'cmd': lambda k:
      ['./run_bayonet.sh', 'bayonet_resilience_sw_%d.bayonet' % (4*k)],
    'envs': [
      { 'EXACT': '' },
      { 'EXACT': 'true' },
    ],
  },
  'probnetkat': {
    'cmd': lambda k:
      ['./run_probnetkat.sh', 'bayonet_resilience_sw_%d.dot' % (4*k)],
    'envs': [
      { 'CPS': 'false', 'PAR': 'true', 'RN': '0' },
      { 'CPS': 'false', 'PAR': 'true', 'RN': '24' },
    ],
  },
  'prism': {
    'cmd': lambda k: ['./run_prism.sh', str(4*k)],
    'envs': [
      { 'EXACT': '' },
      { 'EXACT': 'true' },
    ],
  },
  'prism.compiled': {
    'cmd': lambda k:
      ['./run_prism.compiled.sh', 'bayonet_resilience_sw_%d.dot' % (4*k)],
    'envs': [
      { 'EXACT': '' },
      { 'EXACT': 'true' },
    ],
  },
}


def run():
  errored = { tool : [False for _ in range(len(data['envs']))] 
                    for (tool, data) in tools.items()}
  for i in range(16):
    k = 2**i
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
  run()
