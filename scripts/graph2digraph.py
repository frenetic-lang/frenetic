#!/usr/bin/python

'''This file takes in a topology specification as a DOT graph and creates a file
that is a DOT digraph. This is mainly because ocaml-topology is currently backed
by an ocamlgraph DirectedGraph, but the ocamlgraph DOT parser does not
create a pair of symmetric directed links for each undirected DOT link.
'''

import sys
import argparse
import networkx as nx

def convert(graph):
    digraph = nx.DiGraph()
    for (src,dst,attrs) in graph.edges_iter(data=True):
        digraph.add_edge(src,dst,attrs)

        # Add the symmetric, reversed edge
        revattrs = { 'sport' : attrs['dst_port'], 'dst_port' : attrs['dst_port'],
                     'capacity' : attrs['capacity'], 'cost' : attrs['cost']
                     }
        digraph.add_edge(dst,src,revattrs)

    # Need to add in nodes explicitly, because there might be unconnected nodes
    # in the original
    for node,attrs in graph.nodes_iter(data=True):
        digraph.add_node(node,attrs)

    digraph.graph['name'] = graph.graph['name']
    return digraph

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--in", dest='input', action='store',
                        default=None,
                        help='path to input file (in DOT format)'
                        )
    parser.add_argument("-o", "--out", dest='output', action='store',
                        default=None,
                        help='path to output file'
                        )
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()

    if not args.input:
        print "Need to specify an input file with -i or --in"
        exit(1)

    graph = nx.Graph(nx.read_dot(args.input))
    digraph = convert(graph)

    if not args.output:
        print "You can specify an output file with -o or --out"
        print nx.to_agraph(digraph)
        exit(1)
    else:
        nx.write_dot(digraph,args.output)
