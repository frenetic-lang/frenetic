#!/usr/bin/env python

from mininet.node import OVSSwitch, RemoteController
from mininet.cli import CLI
from mininet.net import Mininet
from mininet.log import lg, info, error, debug, output

def start(ip="127.0.0.1", port=6633):
    # Set up logging etc.
    lg.setLogLevel('info')
    lg.setLogLevel('output')

    ctrlr = lambda n: RemoteController(n, ip=ip, port=port, inNamespace=False)
    net = Mininet(switch=OVSSwitch, controller=ctrlr, listenPort=6635)
    c0 = net.addController('c0')

    # Add hosts
    h1 = net.addHost('h1')
    h2 = net.addHost('h2')
    h3 = net.addHost('h3')

    # Add switches
    s1 = net.addSwitch('s1', dpid='00:00:00:00:00:00:00:01')
    s2 = net.addSwitch('s2', dpid='00:00:00:00:00:00:00:02')
    s3 = net.addSwitch('s3', dpid='00:00:00:00:00:00:00:03')
    s4 = net.addSwitch('s4', dpid='00:00:00:00:00:00:00:04')
    s5 = net.addSwitch('s5', dpid='00:00:00:00:00:00:00:05')
    s6 = net.addSwitch('s6', dpid='00:00:00:00:00:00:00:06')
    s7 = net.addSwitch('s7', dpid='00:00:00:00:00:00:00:07')

    # Add host-switch links
    net.addLink(s5, h1)
    net.addLink(s6, h2)
    net.addLink(s7, h3)

    # Add edge-core links
    net.addLink(s1, s5, 1, 2)
    net.addLink(s3, s6, 2, 2)
    net.addLink(s4, s7, 3, 2)

    # Add internal core links
    net.addLink(s1, s2, 2, 1)
    net.addLink(s1, s4, 3, 1)
    net.addLink(s2, s3, 2, 1)
    net.addLink(s2, s4, 3, 2)

    # Start the network and prime other ARP caches
    net.start()
    net.staticArp()

    # Enter CLI mode
    output("Network ready\n")
    output("Press Ctrl-d or type exit to quit\n")
    CLI(net)
    net.stop()

start()


