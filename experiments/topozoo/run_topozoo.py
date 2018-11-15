#!/usr/bin/env python3

import os
import os.path
import subprocess
import time
import glob

TOPOZOO = "../../examples/topozoo/"  # location of topology files
TIMEOUT = 10 * 60  # 10 minutes
MAXITERS = 1_000_000
FPROB = (1,100)  # probability of link failure
SCHEMES = ["SPF", "ECMP", "RW"] # may add: RW/RWW
PRISM_PCTL_FILE = "prism.pctl"
SETTINGS = [
  { 'cps' : False,  # seems faster
    'dont_iterate' : False,
    'parallelize' : True,
    'prism' : prism,
  } for prism in [True, False]
]


def pnk_cmd(topo, scheme, cps, dont_iterate, parallelize, fbound=None, prism=False):
  num, den = FPROB
  cmd = [
    'probnetkat.with_topo', topo,
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
  settings = settings.copy()
  basename = os.path.basename(settings['topo'])
  basename = os.path.splitext(basename)[0]
  del settings['topo']
  suffix = str(settings)
  for char in "'}{ ":
    suffix = suffix.replace(char, '')
  suffix = suffix.replace(':', '=')
  suffix = suffix.replace(',', '_')
  name = basename + "_" + suffix
  return name


def logfile(settings):
  return "%s.log" % identifier(settings)


def prismfile(settings):
  return "%s.pm" % identifier(settings)

def run_pnk(cmd, log):
  subprocess.run(cmd,
                 timeout=TIMEOUT,
                 check=True,
                 stdout=log, 
                 stderr=log)


def run_prism(settings, pnk_cmd, log):
  prism_file = prismfile(settings)
  with open(prism_file, 'w+') as f:
    subprocess.run(pnk_cmd,
                   timeout=TIMEOUT,
                   check=True,
                   stdout=f,
                   stderr=log)
  cmd = prism_cmd(prism_file)
  print(' '.join(cmd))
  log.write(' '.join(cmd))
  subprocess.run(cmd,
                 timeout=TIMEOUT,
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
      msg = "TIMEOUT: %d seconds.\n" % TIMEOUT
    except subprocess.CalledProcessError as e:
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
    for topo in glob.glob(TOPOZOO + '*.dot'):
      print(topo)
      print("-" * 80)
      for settings in SETTINGS:
        mysettings = settings.copy()
        mysettings['topo'] = topo
        mysettings['scheme'] = scheme
        run_with_settings(mysettings)
    print("\n")
    

if __name__ == "__main__":
  main()
