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
        revattrs = { 'sport' : attrs['dport'], 'dport' : attrs['dport'],
                     'capacity' : attrs['capacity'], 'cost' : attrs['cost']
                     }
        digraph.add_edge(dst,src,revattrs)

    # Need to add in nodes explicitly, because there might be unconnected nodes
    # in the original
    for node,attrs in graph.nodes_iter(data=True):
        digraph.add_node(attrs)

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
    if not args.output:
        print "Need to specify an output file with -o or --out"
        exit(1)

    graph = nx.Graph(nx.read_dot(args.input))
    digraph = convert(graph)
    nx.write_dot(digraph,args.output)
