#!/usr/bin/python
import re
import sys

# Mininet imports
from mininet.log import lg, info, error, debug, output
from mininet.util import quietRun
from mininet.node import  Host, OVSSwitch, RemoteController
from mininet.cli import CLI
from mininet.net import Mininet

from vlanhost import VLANHost

def start(ip="127.0.0.1",port=6633):
    ctrlr = lambda n: RemoteController(n, ip=ip, port=port, inNamespace=False)
    net = Mininet(switch=OVSSwitch, controller=ctrlr, host=VLANHost, autoStaticArp=True)
    c1 = net.addController('c1')

    ####### End of static Mininet prologue ######

    eth1 = '00:00:00:00:00:01'
    eth2 = '00:00:00:00:00:02'
    ip1 = '10.0.0.1'
    ip2 = '10.0.0.2'

    #         -- s2 --
    #        /        \
    # h1 - s1          s3 - h2
    #        \        /
    #         -- s4 --  
    # Ports to host are numbered 1, then clockwise

    s1 = net.addSwitch('s1')
    s2 = net.addSwitch('s2')
    s3 = net.addSwitch('s3')
    s4 = net.addSwitch('s4')

    net.addLink(s1, s2, 2, 1)
    net.addLink(s2, s3, 2, 3)
    net.addLink(s3, s4, 2, 2)
    net.addLink(s4, s1, 1, 3)

    h1 = net.addHost('h1', mac=eth1, ip=ip1)
    h2 = net.addHost('h2', mac=eth2, ip=ip2)
    net.addLink(h1, "s1", 0, 1)
    net.addLink(h2, "s3", 0, 1)

    ###### Start of static Mininet epilogue ######
    # Set up logging etc.
    lg.setLogLevel('info')
    lg.setLogLevel('output')

    # Start the network and prime other ARP caches
    net.start()

    # Enter CLI mode
    output("Network ready\n")
    output("Press Ctrl-d or type exit to quit\n")
    CLI(net)
    net.stop()

start()

