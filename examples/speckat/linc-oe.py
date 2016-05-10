#!/usr/bin/env python

from optical import LINCNet, LINCSwitch, LINCLink

from mininet.node import OVSSwitch, RemoteController
from mininet.cli import CLI
from mininet.net import Mininet
from mininet.log import lg, info, error, debug, output

def start(ip="127.0.0.1",port=6633):

    # Set up logging etc.
    lg.setLogLevel('info')
    lg.setLogLevel('output')

    ctrlr = lambda n: RemoteController(n, ip=ip, port=port, inNamespace=False)
    net = LINCNet(switch=OVSSwitch, controller=ctrlr, autoStaticArp=True, listenPort=6635)
    c1 = net.addController('c1')
    
    # Add hosts
    h1 = net.addHost('h1')
    h2 = net.addHost('h2')
    h3 = net.addHost('h3')
    h4 = net.addHost('h4')

    # Add optical switches
    r1 = net.addSwitch('r1', dpid='00:00:00:00:00:00:00:11', cls=LINCSwitch)
    r2 = net.addSwitch('r2', dpid='00:00:00:00:00:00:00:12', cls=LINCSwitch)
    r3 = net.addSwitch('r3', dpid='00:00:00:00:00:00:00:13', cls=LINCSwitch)
    r4 = net.addSwitch('r4', dpid='00:00:00:00:00:00:00:14', cls=LINCSwitch)

    # Add electrical host-switch links
    net.addLink(h1,r1)
    net.addLink(h2,r2)
    net.addLink(h3,r3)

    # Add optical links
    net.addLink(r1, r4, 2, 1, cls=LINCLink)
    net.addLink(r2, r4, 2, 2, cls=LINCLink)
    net.addLink(r3, r4, 2, 3, cls=LINCLink)

    # Start the network and prime other ARP caches
    net.start()
    net.staticArp()

    # Enter CLI mode
    output("Network ready\n")
    output("Press Ctrl-d or type exit to quit\n")
    CLI(net)
    net.stop()

start()