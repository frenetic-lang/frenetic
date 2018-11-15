import argparse
import glob
import networkx as nx
import sys

from routing import allsp,spf,routing_lib #,disjointtrees

DESTINATION = 's1'

def get_topology(topo_name):
    topo = None
    # Check if the topology already exists - reuse it
    topo_files = glob.glob(topo_name+'.dot')
    if len(topo_files) > 1:
        print "ERROR: Multiple topologies found.", str(topo_files)
    elif len(topo_files) == 1:
        print "Found topology:", topo_files[0]
        topo = nx.drawing.nx_pydot.read_dot(topo_files[0])
    else: 
        # Else generate new topology
        print "ERROR: Topology not found."
    return topo

def routing_trees(topo, routing_alg, dest):
    trees = None
    if routing_alg == 'spf':
        trees = spf.route(topo, dest)
    elif routing_alg == 'allsp':
        trees = allsp.route(topo, dest)
    elif routing_alg == 'disjointtrees':
        trees = disjointtrees.route(topo, dest)
    else:
        print "ERROR: Unknown routing scheme."
    return trees

def network(topo_name, routing_algs):
    print topo_name, routing_algs
    topo = get_topology(topo_name)
    if topo is None:
        print "ERROR: Failed to get topology"
        return

    hosts = []
    num_switches = 0
    for n,d in topo.nodes(data=True):
        if d['type'] == 'host':
            hosts.append(n)
        elif d['type'] == 'switch':
            num_switches += 1

    # Create a graph of only switches
    topo.remove_nodes_from(hosts)

    # Routing. Fix destination. Generate routing tree(s) to this desitnation
    alg_list = routing_algs.split(',')
    for alg in alg_list:
        print "Routing alg:", alg
        routes = routing_trees(topo, alg, DESTINATION)
        routing_lib.serialize_routes(routes, topo_name+'-'+alg)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-t','--topo',type=str,action='store',dest='topo_name',
                        default=None,
                        help='Topology name from topozoo')
    parser.add_argument('-r','--routing',type=str,action='store',dest='routing_algs',
                        default="spf,disjointtrees",
                        help='name of routing algorithms e.g spf')
    parser.add_argument('-o', '--out', action='store',dest='output',
                        default=None,
                        help='file name to write to')
    if len(sys.argv[4:]) == 0:
        parser.print_help()
        sys.exit()

    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    network(args.topo_name, args.routing_algs)
