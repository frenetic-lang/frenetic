#!/usr/bin/python
import socket
from subprocess import Popen, call
from mininet.node import RemoteController, UserSwitch
from mininet.net import Mininet
import mininet.topolib
import mininet.topo
import re
import os
from time import gmtime, strftime

TreeTopo = mininet.topolib.TreeTopo
LinearTopo = mininet.topo.LinearTopo

dev_null = open(os.devnull, 'w')

def is_ipv6_enabled():
  lines = 0
  fh = open("/proc/net/if_inet6")
  return len([l for l in fh]) > 0

class PaneException(Exception):
  pass

def quiet_system(cmd,stdout=dev_null,stderr=dev_null):
  result = call(cmd, shell = True, stdout = stdout, stderr = stderr)

def kill_controllers():
  quiet_system('lsof | grep "TCP \\*:6633 (LISTEN)" | cut -d " " -f 2 | \
    xargs kill')

def time():
  return strftime("%Y-%m-%d-%H%M%S", gmtime())

class MininetRunner(object):

  def __init__(self, topo, dir):
    if os.path.exists(dir):
        raise PaneException("directory %s already exists" % dir)
    os.makedirs(dir)

    self.dir = dir
    self.tcpdump = Popen(["/usr/sbin/tcpdump",
                          "-w", "%s/pcap" % dir, 
                          "-i", "any",
                          "tcp port 6633"])
    self.net = Mininet(topo=topo,
                       controller=RemoteController,
                       switch=UserSwitch,
                       autoSetMacs=True,
                       autoStaticArp=True,
                       cleanup=True)
    self.net.start()
    self.print_net(open(dir + '/topo', 'w'))

  def start_controller(self, user, cmd):
    self.controller_stdout = open('%s/ctrl.stdout' % self.dir, 'w')
    self.controller_stderr = open('%s/ctrl.stderr'  % self.dir, 'w')
    self.controller = Popen([cmd, "%s/topo" % self.dir],
                            stdout=self.controller_stdout,
                            stderr=self.controller_stderr)
    

    

  def print_net(self, f):
    for switch in self.net.switches:
      f.write("%s <->" % switch.name)
      for intf in switch.intfs.values():
        name = switch.connection.get(intf, ( None, 'Unknown ' ) )[ 1 ]
        f.write( ' %s' % name )
      f.write('\n')


  def destroy(self):
    if hasattr(self, 'tcpdump'):
      self.tcpdump.kill()
      delattr(self, 'tcpdump')
    if hasattr(self, 'net'):
      self.net.stop()
      delattr(self, 'net')
    if hasattr(self, 'controller'):
      self.controller.kill()
      delattr(self, 'controller')
      self.controller_stdout.close()
      self.controller_stderr.close()

  def __del__(self):
    self.destroy()

  def ping(self, src, dst, interval, count):
    cmd = 'ping -i %s -c%s %s' % (interval, count, dst.IP())
    out = src.cmd(cmd)
    m = re.search(r"(\d+)% packet loss", out)
    if m == None:
      raise PaneException("%s output was %s" % (cmd, out))
    return int(m.group(1))

