#!/usr/bin/env python

import matplotlib.pyplot as plt
import networkx as nx
from netaddr import EUI, mac_unix, IPAddress

def mk_mac(i):
    mac = EUI(i)
    mac.dialect = mac_unix
    return str(mac)

def mk_ip(i):
    # Generate IPs starting at 111.0.0.0
    ip = IPAddress(1862270976 + i)
    return str(ip)

def draw_graph(G, draw_output):
    pos=nx.spring_layout(G)
    nx.draw(G, pos)
    plt.draw()
    plt.savefig(draw_output)
    plt.clf()
