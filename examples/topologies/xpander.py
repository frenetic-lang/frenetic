#!/usr/bin/env python

'''
Xpander: Towards Optimal-Performance Datacenters, CoNext '16
'''

import argparse
import math
import networkx as nx
import random
from topolib import *

def k_lift(g, k):
    mapping = dict(zip(g.nodes(), [k*x for x in g.nodes()]))
    g = nx.relabel_nodes(g, mapping)
    new_nodes = []
    for n in g.nodes():
        for i in range(1, k):
            new_nodes.append(n+i)
    g.add_nodes_from(new_nodes)
    to_remove_edges = []
    to_add_edges = []
    for u, v in g.edges():
        to_remove_edges.append((u, v))
        matching = list(range(k))
        random.shuffle(matching)
        for i in range(k):
            j = matching[i]
            to_add_edges.append((u+i, v+j))
    for (u, v) in to_remove_edges:
        g.remove_edge(u, v)
    for (u, v) in to_add_edges:
        g.add_edge(u, v)
    return g

def mk_topo(n, k, r, bw='1Gbps', lift_k=2):
    # n switches, each with k ports. r ports are used to connect to other
    # switches.

    # Create a complete r-regular graph of r+1 nodes
    h = nx.random_regular_graph(r, r + 1)
    num_lifts = int(math.ceil(math.log(n / (r+1), lift_k)))
    for lift_i in range(num_lifts):
        h = k_lift(h, lift_k)

    num_switches = int(math.ceil((r + 1) * math.pow(lift_k, num_lifts)))
    assert(nx.number_of_nodes(h) == num_switches)
    num_hosts = num_switches * (k - r)

    hosts = [('h' + str(i), {'type':'host', 'mac':mk_mac(i), 'ip':mk_ip(i)})
             for i in range (1, num_hosts + 1)]

    switches = [('s' + str(i), {'type':'switch', 'level':'edge', 'id':i})
                       for i in range(1,num_switches + 1)]

    switch_labels = dict()
    switch_props_id = dict()
    for i in range(1, num_switches + 1):
        sw = 's' + str(i)
        switch_labels[i-1] = sw
        switch_props_id[sw] = i

    g = nx.relabel_nodes(h.to_directed(), switch_labels)
    nx.set_node_attributes(g, 'type', 'switch')
    nx.set_node_attributes(g, 'id', switch_props_id)
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
