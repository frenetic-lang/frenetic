#!/usr/bin/env python3

import os
import os.path
import subprocess
import time

TIMEOUT = 10 * 60 # 10 minutes
MAXITERS = 1_000_000
FPROB = (1,100)  # probability of link failure
SCHEMES = ["SPF", "ECMP", "RW"] # may add: RW/RWW
PRISM_PCTL_FILE = "prism.pctl"
SETTINGS = [
  { 'cps' : False,  # seems faster
    'dont_iterate' : dont_iterate,
    'parallelize' : parallelize,
    'prism' : False,
  } for dont_iterate, parallelize in [(False, True), (False, False), (True, True)]
]
SETTINGS.append({'prism':True, 'cps':False, 'dont_iterate':False, 'parallelize':True})


def fattree(k):
  return "../../examples/output/fattree_%d_sw_%d.dot" % (k, (5 * k**2)/4)


def pnk_cmd(k, scheme, cps, dont_iterate, parallelize, fbound=None, prism=False):
  num, den = FPROB
  cmd = [
    'probnetkat.with_topo', fattree(k),
    '-scheme', scheme,
    '-fail-with', "%d/%d" % (num, den),
  ]
  if cps:
    cmd.append('-cps')
  if dont_iterate:
    cmd.append('-dont-iterate')
  if not parallelize:
    cmd.append('-no-branch-par')
  if fbound is not None:
    cmd.append('-fbound')
    cmd.append(str(fbound))
  if prism:
    cmd.append('-prism')
  return cmd


def prism_cmd(prism_file):
  return ["prism", prism_file, PRISM_PCTL_FILE, '-maxiters', str(MAXITERS)]


def identifier(settings):
  s = str(settings)
  for char in "'}{ ":
    s = s.replace(char, '')
  s = s.replace(':', '=')
  s = s.replace(',', '_')
  return s


def logfile(settings):
  return "%s.log" % identifier(settings)


def prismfile(settings):
  return "%s.pm" % identifier(settings)

def wrap_cmd(cmd):
  return ['timeout', '-k', '0', str(TIMEOUT)] + cmd

def run_pnk(cmd, log):
  subprocess.run(wrap_cmd(cmd),
                 check=True,
                 stdout=log, 
                 stderr=log)


def run_prism(settings, pnk_cmd, log):
  prism_file = prismfile(settings)
  with open(prism_file, 'w+') as f:
    subprocess.run(wrap_cmd(pnk_cmd),
                   check=True,
                   stdout=f,
                   stderr=log)
  cmd = prism_cmd(prism_file)
  print(' '.join(cmd))
  log.write(' '.join(cmd))
  subprocess.run(wrap_cmd(cmd),
                 check=True,
                 stdout=log,
                 stderr=log)


def run_with_settings(settings):
  print('settings: %s' % str(settings))
  success = False
  msg = ""
  log = logfile(settings)
  if os.path.isfile(log):
    print("-> SKIPPING: %s already exists)\n" % log)
    return True
  with open(log, 'w+') as log:
    try:
      cmd = pnk_cmd(**settings)
      print(' '.join(cmd))
      log.write("$ %s\n" % ' '.join(cmd))
      log.flush()
      t0 = time.perf_counter()
      if settings['prism']:
        run_prism(settings, cmd, log)
      else:
        run_pnk(cmd, log)
      t1 = time.perf_counter()
      msg = "TIME: %f.\n" % (t1 - t0)
      success = True
    except subprocess.TimeoutExpired:
      raise Exception("impossible - we didn't set a timeout")
    except subprocess.CalledProcessError as e:
      # see https://www.gnu.org/software/coreutils/manual/html_node/timeout-invocation.html
      if e.returncode == 124 or e.returncode == 137:
        msg = "TIMEOUT: %d seconds.\n" % TIMEOUT
      else:
        msg = "ERROR: %d.\n" % e.returncode
    finally:
      log.write("\n%s\n" % msg)
      log.close()
  print("-> %s" % msg)
  return success


def main():
  for scheme in SCHEMES:
    print("scheme: %s" % scheme)
    print("=" * 80)
    errored = [False for _ in SETTINGS]
    for k in range(2, 50, 2):
      if all(errored): break
      print("k = %d" % k)
      print("-" * 80)
      for i, settings in enumerate(SETTINGS):
        if errored[i]: continue
        mysettings = settings.copy()
        mysettings['k'] = k
        mysettings['scheme'] = scheme
        errored[i] = not run_with_settings(mysettings)
    print("\n")
    

if __name__ == "__main__":
  main()
