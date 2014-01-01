#!/usr/bin/python

from mininet.net import Mininet
from mininet.node import UserSwitch, RemoteController
from mininet.topo import SingleSwitchTopo
from subprocess import Popen
from sys import stderr

c0 = RemoteController( 'c0', ip='127.0.0.1', port=6633 )

topo = SingleSwitchTopo()
net = Mininet( topo=topo, switch=UserSwitch, build=False )
ld_env = "CAML_LD_LIBRARY_PATH=/home/vagrant/.opam/system/lib/stublibs:/usr/lib/ocaml/stublibs"
net.addController(c0)
net.build()
net.start()

controller = Popen(ld_env + " sudo -u vagrant -E ./_build/test/PingTest.byte",
                    stdout=PIPE,
                    stderr=stderr,
                    shell=True)
# Wait for the controller to signal that it's installed the rules
controller.stdout.readline()

loss = net.ping()
net.stop()
controller.terminate()
controller.wait()
exit(loss)
