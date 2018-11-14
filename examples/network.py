import argparse
import glob
import networkx as nx
import sys

from topologies import abfattree,fattree,jellyfish,vl2,xpander
from routing import allsp,spf,routing_lib #,disjointtrees

DESTINATION = 's1'

def generate_topology(topo_args, topo_name):
    topo = None
    is_new = False
    # Check if the topology already exists - reuse it
    topo_files = glob.glob(topo_name+'*.dot')
    if len(topo_files) > 1:
        print "ERROR: Multiple topologies found.", str(topo_files)
    elif len(topo_files) == 1:
        print "Found topology:", topo_files[0]
        topo = nx.drawing.nx_pydot.read_dot(topo_files[0])
    else: 
      # Else generate new topology
      print "Creating new topology..."
      is_new = True
      targs = topo_args.split(',')
      if targs[0] == 'fattree':
          pods = int(targs[1])
          topo = fattree.mk_topo(pods)
      elif targs[0] == 'abfattree':
          pods = int(targs[1])
          topo = abfattree.mk_topo(pods)
      elif targs[0] == 'vl2':
          da = int(targs[1])
          di = int(targs[2])
          topo = vl2.mk_topo(da, di, rack_size=1, bw='10Gbps')
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
          print "ERROR: Unknown topology."
    return topo, is_new

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

def network(topo_args, routing_algs, topo_name):
    print topo_args, routing_algs
    if topo_name is None:
        topo_name = 'output/'+'_'.join(topo_args.split(','))
    # Generate topology
    topo, is_new = generate_topology(topo_args, topo_name)
    if topo is None:
        print "ERROR: Failed to generate topology"
        return

    hosts = []
    num_switches = 0
    for n,d in topo.nodes(data=True):
        if d['type'] == 'host':
            hosts.append(n)
        elif d['type'] == 'switch':
            num_switches += 1

    # Export the topology
    #topo_name = topo_name + '_sw_' + str(num_switches)
    if is_new:
      nx.drawing.nx_agraph.write_dot(topo, topo_name + '.dot')

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
    parser.add_argument('-t','--topo',type=str,action='store',dest='topo_args',
                        default="fattree,4",
                        help='parameters for topology generation. e.g fattree,4')
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
    network(args.topo_args, args.routing_algs, args.output)
