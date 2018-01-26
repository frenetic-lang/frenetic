#!/usr/bin/env python

'''This file generates VL2 folded-Clos topology in dot notation according to
the paper: "VL2: A Scalable and Flexible Data Center Network" by A. Greenberg
et al (SIGCOMM 2009).
'''
import argparse
import networkx as nx
from topolib import *

def mk_topo(di, da, rack_size, bw):
    num_core_switches = da/2
    num_agg_switches  = di
    num_tor_switches  = (da * di)/4
    num_hosts         = (da * di * rack_size)/4 # num_tor_switches * rack_size

    hosts = [('h' + str(i), {'type':'host', 'mac':mk_mac(i), 'ip':mk_ip(i)})
             for i in range (1, num_hosts + 1)]

    tor_switches = [('s' + str(i), {'type':'switch', 'level':'edge', 'id':i})
                    for i in range(1, num_tor_switches + 1)]

    agg_switches = [('s' + str(i), {'type':'switch', 'level':'aggregation', 'id':i})
                    for i in range(num_tor_switches + 1, num_tor_switches + num_agg_switches+ 1)]

    core_switches = [('s' + str(i), {'type':'switch', 'level':'core', 'id':i})
                       for i in range(num_tor_switches + num_agg_switches + 1,
                                   num_tor_switches + num_agg_switches + num_core_switches + 1)]

    g = nx.DiGraph()
    g.add_nodes_from(hosts)
    g.add_nodes_from(core_switches)
    g.add_nodes_from(agg_switches)
    g.add_nodes_from(tor_switches)

    # Complete bipartite graph between core and aggregation switches
    # Use ports 0 ... di-1 on core switches
    # Use ports 0 ... da/2-1 on aggregation switches
    for core_idx, core_sw in enumerate(core_switches):
        for agg_idx, agg_sw in enumerate(agg_switches):
            core_port = agg_idx
            agg_port = core_idx
            g.add_edge(core_sw[0], agg_sw[0], src_port=core_port,
                       dst_port=agg_port, capacity=bw, cost=1)
            g.add_edge(agg_sw[0], core_sw[0], src_port=agg_port,
                       dst_port=core_port, capacity=bw, cost=1)

    # Connect each ToR switch to two aggregation switches
    # Use ports da/2 ... da-1 on aggregation switches
    # Use ports 0 1 on ToR switches
    for pod in range(num_agg_switches/2):
        pod_agg_switches = agg_switches[pod*2:(pod+1)*2]
        pod_tor_switches = tor_switches[pod*da/2:(pod+1)*da/2]
        for agg_idx, agg_sw in enumerate(pod_agg_switches):
            for tor_idx, tor_sw in enumerate(pod_tor_switches):
                agg_port = da/2 + tor_idx
                tor_port = agg_idx
                g.add_edge(agg_sw[0], tor_sw[0], src_port=agg_port,
                           dst_port=tor_port, capacity=bw, cost=1)
                g.add_edge(tor_sw[0], agg_sw[0], src_port=tor_port,
                           dst_port=agg_port, capacity=bw, cost=1)

    # Connect each ToR switch to rack_size hosts
    # Use ports 2 ... rack_size+1 on ToR switches
    # Use port 0 on each host
    for tor_idx, tor_sw in enumerate(tor_switches):
        # Connect to hosts
        for host_idx in range(rack_size):
            host = hosts[tor_idx*rack_size+host_idx]
            tor_port = 2+host_idx
            host_port = 0
            g.add_edge(tor_sw[0], host[0], src_port=tor_port,
                       dst_port=host_port, capacity='1Gbps', cost=1)
            g.add_edge(host[0], tor_sw[0], src_port=host_port,
                       dst_port=tor_port, capacity='1Gbps', cost=1)
    return g


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-a','--da',type=int,action='store',dest='da',
                        default=4,
                        help='Number of ports in an aggregation switch (parameter D_A in the paper)')
    parser.add_argument('-i','--di',type=int,action='store',dest='di',
                        default=4,
                        help='Number of ports in an Intermediate switch (parameter D_I in the paper)')
    parser.add_argument('-r','--rack_size',type=int,action='store',dest='rack_size',
                        default=2,
                        help='Number of servers connected to a ToR switch')
    parser.add_argument('-b','--bandwidth',type=str,action='store',dest='bw',
                        default='10Gbps',
                        help='bandwidth of each switch-switch link')
    parser.add_argument('-o', '--out', action='store',dest='output',
                        default=None,
                        help='file root to write to')

    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    topo = mk_topo(args.di, args.da, args.rack_size, args.bw)

    if args.output:
        nx.drawing.nx_agraph.write_dot(topo, args.output+'.dot')
        # draw_graph(topo, args.output+".png")
    else:
        print nx.nx_agraph.to_agraph(topo)
