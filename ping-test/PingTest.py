#!/usr/bin/python

from mininet.net import Mininet
from mininet.node import OVSSwitch, RemoteController
from mininet.topo import SingleSwitchTopo
from subprocess import Popen, PIPE
from sys import stderr

ld_env = "CAML_LD_LIBRARY_PATH=/home/vagrant/.opam/system/lib/stublibs:/usr/lib/ocaml/stublibs"
controller = Popen(ld_env + " ./PingTest.byte", stdout=PIPE, stderr=stderr, shell=True)


topo = SingleSwitchTopo()
net = Mininet( topo=topo, switch=OVSSwitch, build=False,controller=RemoteController)
net.build()
net.start()

# Wait for the controller to signal that it's installed the rules
controller.stdout.readline()


loss = net.ping()
net.stop()
controller.terminate()
controller.wait()
exit(loss)
