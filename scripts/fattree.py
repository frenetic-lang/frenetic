#!/usr/bin/python

'''this file generates a simple fat tree in dot notation with few assumptions:
1) the root is always 1 node
2) all switches have same incoming and outgoing edges, except the root node
3) all switches have same fan out '''

import sys
import argparse
import networkx as nx

def generate(fanout,depth):
    switches = ['s'+ str(i) for i in range(1, ((1 - (fanout ** depth)) / (1 - fanout))+1)]
    hosts = ['h' + str(i) for i in range(1, fanout ** depth + 1)]
    nodes = switches + hosts
    g = nx.MultiDiGraph()
    g.add_nodes_from(nodes)

    for i in range(depth,0,-1):
        '''length of temp1 = (length of temp2) * fanout'''
        temp1 = nodes[len(nodes) - (fanout ** i):]
        nodes = nodes[:len(nodes) - (fanout ** i)]
        temp2 = nodes[len(nodes) - (fanout ** (i-1)):]
        for j in range(len(temp2)):
            for k in range(fanout):
                p = fanout ** (depth - i)
                if p == 1:
                    g.add_edge(temp1[fanout * j + k],temp2[j],
                               attr_dict={'sport':1,'dport':k+1,'capacity':'1Gbps','cost':'1'})
                    g.add_edge(temp2[j],temp1[fanout * j + k],
                               attr_dict={'sport':k+1,'dport':1,'capacity':'1Gbps','cost':'1'})
                else:
                    for l in range(p):
                        g.add_edge(temp1[fanout * j + k],temp2[j],
                                   attr_dict={'sport':p+l+1,'dport': fanout * k + l+1,'capacity':'1Gbps','cost':'1'})
                        g.add_edge(temp2[j],temp1[fanout * j + k],
                                   attr_dict={'sport':fanout * k + l+1,'dport': p+l+1,'capacity':'1Gbps','cost':'1'})

    return g

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("fanout", type=int,
                        help="number of children each node should have")
    parser.add_argument("depth", type=int,
                        help="depth of the fattree")
    parser.add_argument("-o", "--out", dest='output', action='store',
                        default=None,
                        help='file to write to')

    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    graph = generate(args.fanout, args.depth)

    if args.output:
        nx.write_dot(graph,args.output)
    else:
        print nx.to_agraph(graph)
