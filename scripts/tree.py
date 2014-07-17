#!/usr/bin/env python

'''this file generates a simple tree in dot notation with few assumptions:
1) the root is always 1 node
2) all switches have same fan out '''

import re
import sys
import argparse
import networkx as nx
from netaddr import EUI, mac_unix, IPAddress

def multiply(depth, bw,mult):
    r = re.compile('(\d+)([KMGT]?[Bb]ps)')
    m = r.match(bw)
    num = m.group(1)
    rate = m.group(2)
    if depth == 0:
        coeff = int(num)
    else:
        coeff = int(num) * depth * mult
    return str(coeff) + rate

def mk_mac(i):
    mac = EUI(i)
    mac.dialect = mac_unix
    return str(mac)

def mk_ip(i):
    # Generate IPs starting at 111.0.0.0
    ip_base = 1862270976
    ip = IPAddress(ip_base + i)
    return str(ip)

def mk_topo(fanout,depth,bw,mult):
    num_hosts = fanout ** depth
    num_switches = (1 - (fanout ** depth)) / (1 - fanout)

    switches = [('s' + str(i), {'type':'switch','id':i})
                  for i in range(1,num_switches + 1)]

    hosts = [('h' + str(i), {'type':'host', 'mac':mk_mac(i), 'ip':mk_ip(i)})
             for i in range (1, num_hosts + 1)]

    nodes = switches + hosts

    g = nx.DiGraph()
    g.add_nodes_from(switches)
    g.add_nodes_from(hosts)

    offset = 0
    parent_offset = 0
    for d in range(depth+1):
        num_nodes = fanout ** d
        if num_nodes == 1:
            parent_offset = offset
            offset += num_nodes
            continue
        parents = nodes[parent_offset:offset]
        currents = nodes[offset:(offset+num_nodes)]
        for i in range(len(parents)):
            parent = parents[i][0]
            for j in range(fanout):
                child = currents[i*fanout + j][0]
                cap = multiply(depth-i,bw, mult)
                # Each node connects to its parent on port 0
                g.add_edge(parent,child,
                           {'src_port':j+1,'dst_port':0,'capacity':cap,'cost':'1'})
                g.add_edge(child, parent,
                           {'src_port':0,'dst_port':j+1,'capacity':cap,'cost':'1'})
        parent_offset = offset
        offset += num_nodes
    return g

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-f',"--fanout", type=int, action='store',dest='fanout',
                        help="number of children each node should have")
    parser.add_argument('-d',"--depth", type=int,action='store',dest='depth',
                        help="depth of the fattree")
    parser.add_argument('-b','--bandwidth',type=str,action='store',dest='bw',
                        default="1Gbps",
                        help='bandwidth of each link')
    parser.add_argument('-m','--multiplier',type=int,action='store',dest='mult',
                        default=1,
                        help='multiplier for bandwidth at each level')
    parser.add_argument("-o", "--out", dest='output', action='store',
                        default=None,
                        help='file root to write to')

    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    topo = mk_topo(args.fanout, args.depth, args.bw, args.mult)

    if args.output:
        nx.write_dot(topo,args.output + '.dot')
    else:
        print nx.to_agraph(topo)
