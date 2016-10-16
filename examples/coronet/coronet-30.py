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
    net = LINCNet(switch=OVSSwitch, controller=ctrlr, listenPort=6635)
    c1 = net.addController('c1')

    ####### End of static Mininet prologue ######

    Charlotte = net.addSwitch('s1', dpid='00:00:00:00:00:01', cls=LINCSwitch)
    Miami = net.addSwitch('s2', dpid='00:00:00:00:00:02', cls=LINCSwitch)
    Atlanta = net.addSwitch('s3', dpid='00:00:00:00:00:03', cls=LINCSwitch)
    Houston = net.addSwitch('s4', dpid='00:00:00:00:00:04', cls=LINCSwitch)
    StLouis = net.addSwitch('s5', dpid='00:00:00:00:00:05', cls=LINCSwitch)
    Indianapolis = net.addSwitch('s6', dpid='00:00:00:00:00:06', cls=LINCSwitch)
    Seattle = net.addSwitch('s7', dpid='00:00:00:00:00:07', cls=LINCSwitch)
    Sacramento = net.addSwitch('s8', dpid='00:00:00:00:00:08', cls=LINCSwitch)
    WashingtonDC = net.addSwitch('s9', dpid='00:00:00:00:00:09', cls=LINCSwitch)
    Baltimore = net.addSwitch('s10', dpid='00:00:00:00:00:0a', cls=LINCSwitch)
    Philadelphia = net.addSwitch('s11', dpid='00:00:00:00:00:0b', cls=LINCSwitch)
    Denver = net.addSwitch('s12', dpid='00:00:00:00:00:0c', cls=LINCSwitch)
    SaltLakeCity = net.addSwitch('s13', dpid='00:00:00:00:00:0d', cls=LINCSwitch)
    SanFrancisco = net.addSwitch('s14', dpid='00:00:00:00:00:0e', cls=LINCSwitch)
    Cleveland = net.addSwitch('s15', dpid='00:00:00:00:00:0f', cls=LINCSwitch)
    Boston = net.addSwitch('s16', dpid='00:00:00:00:00:10', cls=LINCSwitch)
    SanJose = net.addSwitch('s17', dpid='00:00:00:00:00:11', cls=LINCSwitch)
    LosAngeles = net.addSwitch('s18', dpid='00:00:00:00:00:12', cls=LINCSwitch)
    Chicago = net.addSwitch('s19', dpid='00:00:00:00:00:13', cls=LINCSwitch)
    ElPaso = net.addSwitch('s20', dpid='00:00:00:00:00:14', cls=LINCSwitch)
    NewYork = net.addSwitch('s21', dpid='00:00:00:00:00:15', cls=LINCSwitch)
    NewOrleans = net.addSwitch('s22', dpid='00:00:00:00:00:16', cls=LINCSwitch)
    Parkersburg = net.addSwitch('s23', dpid='00:00:00:00:00:17', cls=LINCSwitch)
    KansasCity = net.addSwitch('s24', dpid='00:00:00:00:00:18', cls=LINCSwitch)
    Dallas = net.addSwitch('s25', dpid='00:00:00:00:00:19', cls=LINCSwitch)
    LasVegas = net.addSwitch('s26', dpid='00:00:00:00:00:1a', cls=LINCSwitch)
    Phoenix = net.addSwitch('s27', dpid='00:00:00:00:00:1b', cls=LINCSwitch)
    SanAntonio = net.addSwitch('s28', dpid='00:00:00:00:00:1c', cls=LINCSwitch)
    Tampa = net.addSwitch('s29', dpid='00:00:00:00:00:1d', cls=LINCSwitch)
    Tallahassee = net.addSwitch('s30', dpid='00:00:00:00:00:1e', cls=LINCSwitch)
    net.addLink(Charlotte, Miami, 0, 0, cls=LINCLink)
    net.addLink(Miami, Tampa, 1, 0, cls=LINCLink)
    net.addLink(Atlanta, Charlotte, 1, 1, cls=LINCLink)
    net.addLink(Atlanta, Houston, 0, 0, cls=LINCLink)
    net.addLink(Houston, SanAntonio, 3, 1, cls=LINCLink)
    net.addLink(StLouis, Indianapolis, 0, 0, cls=LINCLink)
    net.addLink(Indianapolis, Parkersburg, 2, 1, cls=LINCLink)
    net.addLink(Seattle, Sacramento, 0, 0, cls=LINCLink)
    net.addLink(Seattle, SaltLakeCity, 1, 3, cls=LINCLink)
    net.addLink(Sacramento, SanFrancisco, 1, 0, cls=LINCLink)
    net.addLink(WashingtonDC, Charlotte, 0, 2, cls=LINCLink)
    net.addLink(Baltimore, WashingtonDC, 1, 1, cls=LINCLink)
    net.addLink(Baltimore, Philadelphia, 0, 0, cls=LINCLink)
    net.addLink(Denver, SaltLakeCity, 0, 0, cls=LINCLink)
    net.addLink(Denver, ElPaso, 1, 0, cls=LINCLink)
    net.addLink(SaltLakeCity, Sacramento, 2, 2, cls=LINCLink)
    net.addLink(SaltLakeCity, LasVegas, 1, 0, cls=LINCLink)
    net.addLink(SanFrancisco, SanJose, 1, 1, cls=LINCLink)
    net.addLink(Cleveland, Boston, 0, 0, cls=LINCLink)
    net.addLink(Boston, NewYork, 1, 0, cls=LINCLink)
    net.addLink(SanJose, LosAngeles, 0, 0, cls=LINCLink)
    net.addLink(LosAngeles, Phoenix, 1, 1, cls=LINCLink)
    net.addLink(Chicago, Indianapolis, 1, 1, cls=LINCLink)
    net.addLink(Chicago, Cleveland, 0, 1, cls=LINCLink)
    net.addLink(ElPaso, Phoenix, 1, 0, cls=LINCLink)
    net.addLink(ElPaso, SanAntonio, 2, 0, cls=LINCLink)
    net.addLink(NewYork, Philadelphia, 1, 1, cls=LINCLink)
    net.addLink(NewOrleans, Houston, 0, 1, cls=LINCLink)
    net.addLink(NewOrleans, Tallahassee, 1, 0, cls=LINCLink)
    net.addLink(Parkersburg, WashingtonDC, 0, 2, cls=LINCLink)
    net.addLink(KansasCity, StLouis, 2, 1, cls=LINCLink)
    net.addLink(KansasCity, Denver, 1, 2, cls=LINCLink)
    net.addLink(KansasCity, Dallas, 0, 0, cls=LINCLink)
    net.addLink(Dallas, Houston, 1, 2, cls=LINCLink)
    net.addLink(LasVegas, Phoenix, 1, 2, cls=LINCLink)
    net.addLink(Tampa, Tallahassee, 1, 1, cls=LINCLink)

    ###### Start of static Mininet epilogue ######
    # Set up logging etc.
    lg.setLogLevel('info')
    lg.setLogLevel('output')

    # Start the network and prime other ARP caches
    net.start()
    net.staticArp()

    # Enter CLI mode
    output("Network ready\n")
    output("Press Ctrl-d or type exit to quit\n")
    CLI(net)
    net.stop()

start()

