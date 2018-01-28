#!/usr/bin/env python

'''F10:  A Fault-Tolerant Engineered Network, NSDI '13
'''
import argparse
import networkx as nx
from topolib import *

def mk_topo(k, bw='1Gbps'):
    '''
    k : number of ports on each switch.
    p = k/2 ports each for uplink (p to k-1) and downlink (0 to p)
    '''
    p = k/2
    num_hosts           = 2 * (p ** (2+1))
    pods                = 2 * p
    num_core_switches   = p ** 2
    num_agg_switches    = 2 * (p ** 2)
    num_edge_switches   = 2 * (p ** 2)

    hosts = [('h' + str(i), {'type':'host', 'mac':mk_mac(i), 'ip':mk_ip(i)})
             for i in range (1, num_hosts + 1)]

    edge_switches = [('s' + str(i), {'type':'switch', 'level':'edge', 'id':i})
                    for i in range(1, num_edge_switches + 1)]

    agg_switches = [('s' + str(i), {'type':'switch', 'level':'aggregation', 'id':i})
                    for i in range(num_edge_switches + 1, num_edge_switches + num_agg_switches+ 1)]

    core_switches = [('s' + str(i), {'type':'switch', 'level':'core', 'id':i})
                       for i in range(num_edge_switches + num_agg_switches + 1,
                                   num_edge_switches + num_agg_switches + num_core_switches + 1)]


    g = nx.DiGraph()
    g.add_nodes_from(hosts)
    g.add_nodes_from(core_switches)
    g.add_nodes_from(agg_switches)
    g.add_nodes_from(edge_switches)
    core_ports = dict()
    for core in core_switches:
      core_ports[core[0]] = 0

    host_offset = 0
    for pod in range(pods):
        pod_agg_switches = agg_switches[pod*pods/2:(pod+1)*pods/2]
        pod_edge_switches = edge_switches[pod*pods/2:(pod+1)*pods/2]
        up_port = pods/2
        for agg in pod_agg_switches:
            # connect aggregate and edge switches
            down_port = 0
            for edge in pod_edge_switches:
                g.add_edge(agg[0],edge[0],
                           src_port=down_port, dst_port=up_port, capacity=bw, cost=1)
                g.add_edge(edge[0],agg[0],
                           src_port=up_port, dst_port=down_port, capacity=bw, cost=1)
                down_port += 1
            up_port += 1

        # connect aggregate and core switches
        if pod%2 == 0:
            # Type A
            j = 0
            for agg in pod_agg_switches:
                cores = core_switches[j*pods/2:(j+1)*pods/2]
                for core in cores:
                    core_port = core_ports[core[0]]
                    print core_port
                    g.add_edge(agg[0],core[0],
                           src_port=pods+j, dst_port=core_port, capacity=bw, cost=1)
                    g.add_edge(core[0],agg[0],
                           src_port=core_port, dst_port=pods+j, capacity=bw, cost=1)
                    core_ports[core[0]] += 1
                j += 1
        else:
            # Type B
            j = 0
            for agg in pod_agg_switches:
                core_sw_idxs = [j + x * p for x in range(pods/2)]
                cores = [core_switches[x] for x in core_sw_idxs]
                for core in cores:
                    core_port = core_ports[core[0]]
                    print core_port
                    g.add_edge(agg[0],core[0],
                           src_port=pods+j, dst_port=core_port, capacity=bw, cost=1)
                    g.add_edge(core[0],agg[0],
                           src_port=core_port, dst_port=pods+j, capacity=bw, cost=1)
                    core_ports[core[0]] += 1
                j += 1

    for idx in range(len(edge_switches)):
        edge = edge_switches[idx]
        # Connect to hosts
        for port in range(pods/2):
            host_idx = idx*(pods/2) + port
            host = hosts[host_idx][0]
            # All hosts connect on port 0
            g.add_edge(edge[0],host,
                       src_port=port, dst_port=0, capacity=bw, cost=1)
            g.add_edge(host,edge[0],
                       src_port=0, dst_port=port, capacity=bw, cost=1)
    return g

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-k','--pods',type=int,action='store',dest='pods',
                        default=4,
                        help='number of ports on each switch (= #pods)')
    parser.add_argument('-b','--bandwidth',type=str,action='store',dest='bw',
                        default='1Gbps',
                        help='bandwidth of each link')
    parser.add_argument('-o', '--out', action='store',dest='output',
                        default=None,
                        help='file root to write to')

    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    topo = mk_topo(args.pods,args.bw)

    if args.output:
        nx.drawing.nx_agraph.write_dot(topo, args.output+'.dot')
        # draw_graph(topo, args.output+".png")
    else:
        print nx.nx_agraph.to_agraph(topo)
