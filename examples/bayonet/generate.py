""" Script for generating Bayonet/ProbNetKAT code for the reliability experiment from the Bayonet
    paper.
"""

import argparse
import glob
import networkx as nx
import sys
import os
import matplotlib.pyplot as plt
from string import Template
import struct
import socket
import schedulers

# helpers
def int2ip(addr):
    return socket.inet_ntoa(struct.pack("!I", addr))

def int2mac(addr):
    mac = "%012x" % addr
    return ':'.join(mac[i:i+2] for i in range(0,12,2))


def make_topo(k):
  # map node id to node
  def node(id):
    if id == -1:
      return "H0"
    if id == k*4:
      return "H1"
    return "S%d" % id

  G = nx.Graph()
  # initial edge is special case
  G.add_edge(node(-1), node(0), src_port = 1, dst_port = 1)

  # all other edges are regular
  for m in range(k):
    n0 = node(m*4 + 0)
    n1 = node(m*4 + 1)
    n2 = node(m*4 + 2)
    n3 = node(m*4 + 3)
    n4 = node(m*4 + 4)
    G.add_edges_from([
      (n0, n1, {'src_port': 2, 'dst_port': 1}),
      (n0, n2, {'src_port': 3, 'dst_port': 1}),
      (n1, n3, {'src_port': 2, 'dst_port': 1}),
      (n2, n3, {'src_port': 2, 'dst_port': 2}),
      (n3, n4, {'src_port':3, 'dst_port': 1})
    ])
    for i,n in enumerate([n0, n1, n2, n3]):
      G.nodes[n]['type'] = "switch"
      G.nodes[n]['id'] = m*4 + i
  G.nodes[node(-1)]['type'] = "host"
  G.nodes[node(-1)]['id'] = -1
  G.nodes[node(k*4)]['type'] = "host"
  G.nodes[node(k*4)]['id'] = k*4

  # add IP and mac addresses
  for n, attrs in G.nodes.items():
    attrs['ip'] = int2ip(attrs['id'] % (2**24 - 2) + 10 * 2**24)
    attrs['mac'] = int2mac(attrs['id'] % (2**24 - 2) + 10 * 2**24)

  # nx.draw(G)
  # plt.show()
  return G


def make_bayonet(k):
  G = make_topo(k)
  params = dict()

  # number of steps
  params['num_steps'] = k * 8 + 2

  # nodes
  # nodes = []
  # for n in G.nodes:
  #   nodes.append(l(n))
  params['nodes'] = ', '.join(sorted(G.nodes))

  # links
  links = []
  for (s,d), e in G.edges.items():
    # all edges must be interpreted to go from node with lower id to node with higher id
    s,d = sorted([s,d], key=lambda n: G.nodes[n]['id'])
    links.append("(%s,pt%d) <-> (%s,pt%d)" % (s, e['src_port'], d, e['dst_port']))
  params['links'] = ',\n    '.join(sorted(links))

  # programs
  programs = []
  for n,attr in G.nodes.items():
    if attr['id'] in range(k*4):
      programs.append("%s -> s%d" % (n, attr['id'] % 4))
  params['programs'] = ', '.join(sorted(programs))

  # scheduler
  params['scheduler'] = schedulers.uniform

  t = Template("""
num_steps $num_steps;

topology{
  nodes{ $nodes }
  links{
    $links
  }
}

programs{ H0 -> h0, H1 -> h1,
  $programs
}

query probability(arrived@H1);

packet_fields{ }

def h0(){
  fwd(1);
}
def h1() state arrived(0){
  arrived=1;
  drop;
}
def s0(){
  if flip(1/2){
    fwd(2);
  }else{
    fwd(3);
  }
}
def s1(){
  fwd(2);
}
def s2()state failing(2){
  if failing == 2 { failing = flip(1/1000); }
  if failing == 1 { drop }
  else{ fwd(2); }
}
def s3(){
  fwd(3);
}

$scheduler
""")
  return t.substitute(params)


def make_dot(k, file):
  G = make_topo(k)
  topo = nx.DiGraph()
  for n, attrs in G.nodes.items():
    topo.add_node(n, **attrs)
  for (s,d), attrs in G.edges.items():
    # all edges must be interpreted to go from node with lower id to node with higher id
    s,d = sorted([s,d], key=lambda n: G.nodes[n]['id'])
    topo.add_edge(s,d, **attrs)

    # this is a bidirectional link
    s,d = d,s
    attrs['src_port'], attrs['dst_port'] = attrs['dst_port'], attrs['src_port']
    topo.add_edge(s,d, **attrs)
  nx.drawing.nx_agraph.write_dot(topo, file)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--output',type=str,action='store',dest='dir', default=".",
                        help='directory to write the output files to')
    # parser.add_argument('-m','--multiplier',type=int,action='store',dest='multiplier',
    #                     default="1",
    #                     help='multiplier to scale the size of the topology')
    return parser.parse_args()

def write_to(file, s):
  with open(file, 'w') as f:
    f.write(s)

if __name__ == '__main__':
    args = parse_args()
    for multiplier in [2**n for n in range(12)]:
      base_filename = "bayonet_resilience_sw_%d" % (multiplier * 4)
      dot_file = os.path.join(args.dir, base_filename + ".dot")
      bayonet_file = os.path.join(args.dir, base_filename + ".bayonet")
      make_dot(multiplier, dot_file)
      write_to(bayonet_file, make_bayonet(multiplier))
