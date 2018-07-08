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
    for k,n in enumerate([n0, n1, n2, n3]):
      G.nodes[n]['type'] = "switch"
      G.nodes[n]['id'] = m*4 + k
  G.nodes[node(-1)]['type'] = "host"
  G.nodes[node(-1)]['id'] = -1
  G.nodes[node(k*4)]['type'] = "host"
  G.nodes[node(k*4)]['id'] = k*4
  # nx.draw(G)
  # plt.show()
  return G


def make_bayonett(k):
  G = make_topo(k)
  params = dict()

  # number of steps
  params['num_steps'] = k * 8 + 2

  # nodes
  # nodes = []
  # for n in G.nodes:
  #   nodes.append(l(n))
  params['nodes'] = ', '.join(G.nodes)

  # links
  links = []
  for (s,d), e in G.edges.iteritems():
    # all edges must be interpreted to go from node with lower id to node with higher id
    s,d = sorted([s,d], key=lambda n: G.nodes[n]['id'])
    links.append("(%s,pt%d) <-> (%s,pt%d)" % (s, e['src_port'], d, e['dst_port']))
  params['links'] = ',\n'.join(links)

  # programs
  programs = []
  for n,attr in G.nodes.iteritems():
    if attr['id'] in range(k*4):
      programs.append("%s -> s%d" % (n, attr['id'] % 4))
  params['programs'] = ', '.join(programs)

  t = Template("""
num_steps $num_steps;

topology{
  nodes{ $nodes }
           
  links{ $links }
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


def scheduler() state phase(0), cur_node(0){ // Phase 0: Execute RunSw, Phase 1: Exectue FwdQ
  for p in [0..2){
    if phase == 0{
      for i in [0..k){
        if (Q_in@cur_node).size() > 0{
          return (RunSw,cur_node);
        }
        cur_node = (cur_node + 1) % k;
      }
      phase = 1;
      cur_node = 0;
    }
    if phase == 1{
      for i in [0..k){
        if (Q_out@cur_node).size() > 0{
          return (FwdQ,cur_node);
        }
        cur_node = (cur_node + 1) % k;
      }
      phase = 0;
      cur_node = 0;
    }
  }
  assert(0);
}
""")
  return t.substitute(params)


def make_dot(k, file):
  G = make_topo(k)
  topo = nx.DiGraph()
  for n, attrs in G.nodes.iteritems():
    topo.add_node(n, **attrs)
  for (s,d), attrs in G.edges.iteritems():
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
    parser.add_argument('-m','--multiplier',type=int,action='store',dest='multiplier',
                        default="1",
                        help='multiplier to scale the size of the topology')
    return parser.parse_args()

if __name__ == '__main__':
    args = parse_args()
    base_filename = "resilience-%d" % args.multiplier
    dot_file = os.path.join(args.dir, base_filename + ".dot")
    # print make_bayonett(args.multiplier)
    # make_topo(args.multiplier)
    make_dot(args.multiplier, dot_file)
