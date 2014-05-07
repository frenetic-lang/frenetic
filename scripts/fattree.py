#!/usr/bin/env python

'''This file generates clos-style fattrees in dot notation according to the
paper: A Scalable Commodity Data Center Network Architecture by Al-Fares,
Loukissas and Vahdat.
'''

import argparse
import networkx as nx

def mk_topo(pods, bw):
    num_hosts         = (pods ** 3)/4
    num_agg_switches  = pods * pods
    num_core_switches = (pods * pods)/4

    hosts = ['h' + str(i) for i in range (1, num_hosts + 1)]
    core_switch_ids = [('s' + str(i), {'id':i}) for i in range(1,num_core_switches + 1)]
    core_switches = [swid[0] for swid in core_switch_ids]
    agg_switch_ids = [('s' + str(i), {'id':i})
                      for i in range(num_core_switches + 1,num_core_switches + num_agg_switches+ 1)]
    agg_switches = [swid[0] for swid in agg_switch_ids]

    g = nx.DiGraph()
    g.add_nodes_from(hosts,type='host')
    g.add_nodes_from(core_switch_ids, type='switch')
    g.add_nodes_from(agg_switch_ids, type='switch')

    host_offset = 0
    for pod in range(pods):
        core_offset = 0
        for sw in range(pods/2):
            switch = agg_switches[(pod*pods) + sw]
            # Connect to core switches
            for port in range(pods/2):
                core_switch = core_switches[core_offset]
                g.add_edge(switch,core_switch,
                           {'src_port':port,'dst_port':pod,'capacity':bw,'cost':1})
                g.add_edge(core_switch,switch,
                           {'src_port':pod,'dst_port':port,'capacity':bw,'cost':1})
                core_offset += 1

            # Connect to aggregate switches in same pod
            for port in range(pods/2,pods):
                lower_switch = agg_switches[(pod*pods) + port]
                g.add_edge(switch,lower_switch,
                           {'src_port':port,'dst_port':sw,'capacity':bw,'cost':1})
                g.add_edge(lower_switch,switch,
                           {'src_port':sw,'dst_port':port,'capacity':bw,'cost':1})

        for sw in range(pods/2,pods):
            switch = agg_switches[(pod*pods) + sw]
            # Connect to hosts
            for port in range(pods/2,pods): # First k/2 pods connect to upper layer
                host = hosts[host_offset]
                # All hosts connect on port 0
                g.add_edge(switch,host,
                           {'src_port':port,'dst_port':0,'capacity':bw,'cost':1})
                g.add_edge(host,switch,
                           {'src_port':0,'dst_port':port,'capacity':bw,'cost':1})
                host_offset += 1

    return g

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p','--pods',type=int,action='store',dest='pods',
                        default=4,
                        help='number of pods (parameter k in the paper)')
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
        nx.write_dot(topo, args.output+'.dot')
    else:
        print nx.to_agraph(topo)
