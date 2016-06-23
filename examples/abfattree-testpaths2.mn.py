
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

    ctrlr = lambda n: RemoteController(n, defaultIP=ip, port=port, inNamespace=False)
    net = Mininet(switch=OVSSwitch, controller=ctrlr, host=VLANHost, autoStaticArp=True)
    c1 = net.addController('c1')

    ####### End of static Mininet prologue ######

    h1 = net.addHost('h1', mac='00:00:00:00:00:01', ip='10.0.0.1')
    h2 = net.addHost('h2', mac='00:00:00:00:00:02', ip='10.0.0.2')
    h3 = net.addHost('h3', mac='00:00:00:00:00:03', ip='10.0.0.3')
    h4 = net.addHost('h4', mac='00:00:00:00:00:04', ip='10.0.0.4')
    s1 = net.addSwitch('s1')
    s10 = net.addSwitch('s10')
    s2 = net.addSwitch('s2')
    s9 = net.addSwitch('s9')
    net.addLink(h1, s1, 1, 1)
    net.addLink(h2, s1, 1, 2)
    net.addLink(h3, s2, 1, 1)
    net.addLink(h4, s2, 1, 2)
    net.addLink(s1, s9, 3, 1)
    net.addLink(s1, s10, 4, 1)
    net.addLink(s2, s9, 3, 2)
    net.addLink(s2, s10, 4, 2)

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

