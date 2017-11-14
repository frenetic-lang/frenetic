import argparse
import networkx as nx
import sys
from sage.all import *

from topologies import fattree,jellyfish,xpander

def resilient_routing(topo_args, topo_name):
    targs = topo_args.split(',')
    # Fetch topology
    if targs[0] == 'fattree':
        pods = int(targs[1])
        topo = fattree.mk_topo(pods)
    elif targs[0] == 'jellyfish':
        n = int(targs[1])
        k = int(targs[2])
        r = int(targs[3])
        topo = jellyfish.mk_topo(n, k, r)
    elif targs[0] == 'xpander':
        n = int(targs[1])
        k = int(targs[2])
        r = int(targs[3])
        topo = xpander.mk_topo(n, k, r)
    else:
        print "Unknown topology."
        return

    # Export the topology
    nx.drawing.nx_agraph.write_dot(topo, topo_name + '.dot')

    # Remove hosts. Redundant operations
    hosts = []
    for n,d in topo.nodes(data=True):
        if d['type'] == 'host':
            hosts.append(n)
    topo.remove_nodes_from(hosts)

    # Convert to SAGE graph
    G = DiGraph(topo)

    e_con = G.edge_connectivity()
    if e_con < 2:
        print "Graph is not 2-connected. Aborting."
        sys.exit()

    # Select root
    root = G.vertices()[0]

    arborescences = G.edge_disjoint_spanning_trees(2, root=root, solver='GLPK')

    counter = 0
    with open(topo_name + '.trees', 'w') as f:
        for a in arborescences:
            f.write("# TREE " + str(counter) + '\n')
            for u,v,d in a.edges():
                f.write(str(u) + ' -- ' + str(v) + '\n')
            counter += 1


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-t','--topo',type=str,action='store',dest='topo_args',
                        default="fattree,4",
                        help='parameters for topology generation. e.g fattree,4')
    parser.add_argument('-o', '--out', action='store',dest='output',
                        default=None,
                        help='file name to write to')
    if len(sys.argv[1:]) == 0:
        parser.print_help()
        sys.exit()


    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    resilient_routing(args.topo_args, args.output)
