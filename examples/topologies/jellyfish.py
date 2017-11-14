#!/usr/bin/env python

'''
Jellyfish: Networking Data Centers Randomly, NSDI '12
Based on interpretation of https://people.inf.ethz.ch/asingla/papers/jellyfish-nsdi12.pdf 's Section 3
'''

import argparse
import networkx as nx
from topolib import *

def mk_topo(n, k, r, bw):
    # n switches, each with k ports. r ports are used to connect to other
    # switches.
    num_hosts    = n * (k - r)
    num_switches = n

    hosts = [('h' + str(i), {'type':'host', 'mac':mk_mac(i), 'ip':mk_ip(i)})
             for i in range (1, num_hosts + 1)]

    switches = [('s' + str(i), {'type':'switch','id':i})
                       for i in range(1,num_switches + 1)]

    switch_labels = dict()
    switch_props = dict()
    for i in range(1, num_switches + 1):
        sw = 's' + str(i)
        switch_labels[i-1] = sw
        switch_props[sw] = {'type':'switch','id':i}

    # Create random regular graph of ToRs
    h = nx.random_regular_graph(r, n)
    g = nx.relabel_nodes(h.to_directed(), switch_labels)
    nx.set_node_attributes(g, switch_props)
    # Add hosts
    g.add_nodes_from(hosts)
    for sw in g.nodes():
        counter = 0
        for u,v,d in g.out_edges(sw,data=True):
            d['src_port'] = counter
            d['capacity'] = bw
            d['cost'] = 1
            counter += 1
        counter = 0
        for u,v,d in g.in_edges(sw,data=True):
            d['dst_port'] = counter
            counter += 1


    for (i,sw) in switch_labels.iteritems():
        for rack_host in range(k - r):
            host = hosts[i * (k - r) + rack_host][0]
            g.add_edge(sw, host,
                       src_port=(k - r) + rack_host, dst_port=0, capacity=bw, cost=1)
            g.add_edge(host, sw,
                       src_port=0, dst_port=(k-r) + rack_host, capacity=bw, cost=1)
    return g

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-n','--numswitches',type=int,action='store',dest='n',
                        default=8,
                        help='number of servers')
    parser.add_argument('-k','--portcount',type=int,action='store',dest='k',
                        default=32,
                        help='number of ports per switch')
    parser.add_argument('-r','--switchdegree',type=int,action='store',dest='r',
                        default=4,
                        help='degree of each switch')
    parser.add_argument('-b','--bandwidth',type=str,action='store',dest='bw',
                        default='1Gbps',
                        help='bandwidth of each link')
    parser.add_argument('-o', '--out', action='store',dest='output',
                        default=None,
                        help='file root to write to')

    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    topo = mk_topo(args.n, args.k, args.r, args.bw)

    if args.output:
        nx.drawing.nx_agraph.write_dot(topo, args.output+'.dot')
        # draw_graph(topo, args.output+".png")
    else:
        print nx.nx_agraph.to_agraph(topo)
