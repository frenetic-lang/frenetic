from mininet.net import Mininet
from mininet.node import RemoteController, OVSSwitch
from functools import partial

net = Mininet(switch=partial(OVSSwitch, protocols="openflow13"))
net.addController( 'c0', controller=RemoteController, port=6633)

h1 = net.addHost("h1", mac='00:00:00:00:00:01')
h2 = net.addHost("h2", mac='00:00:00:00:00:02')

s0 = net.addSwitch("s1")
s1 = net.addSwitch("s2")
s2 = net.addSwitch("s3")
s3 = net.addSwitch("s4")

# path top
net.addLink(h1, s0, 1, 3)
failme = net.addLink(s0, s1, 1, 2)
net.addLink(s1, s3, 1, 1)
net.addLink(s3, h2, 3, 1)

#path bottom
net.addLink(s3, s2, 2, 1)
net.addLink(s2, s0, 2, 2)

net.start()

net.pingAll()

failme.delete()

net.pingAll()

net.stop()
