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
  G = nx.Graph()
  # initial edge is special case
  G.add_edge(-1, 0, src_port = 1, dst_port = 1)
  for m in range(k):
    n0 = m*4
    n1 = m*4 + 1
    n2 = m*4 + 2
    n3 = m*4 + 3
    G.add_edges_from([
      (n0, n1, {'src_port': 2, 'dst_port': 1}),
      (n0, n2, {'src_port': 3, 'dst_port': 1}),
      (n1, n3, {'src_port': 2, 'dst_port': 1}),
      (n2, n3, {'src_port': 2, 'dst_port': 2}),
      (n3, n3+1, {'src_port':3, 'dst_port': 1})
    ])
    for n in [n0, n1, n2, n3]:
      G.nodes[n]['type'] = "switch"
  G.nodes[-1]['type'] = "host"
  G.nodes[k*4]['type'] = "host"
  # nx.draw(G)
  # plt.show()
  return G


def make_bayonett(k):
  def l(i):
    if i == -1:
      return "H0"
    if i == k*4:
      return "H1"
    return "S%d" % i
  G = make_topo(k)
  params = dict()

  # number of steps
  params['num_steps'] = k * 8 + 2

  # nodes
  nodes = []
  for n in G.nodes:
    nodes.append(l(n))
  params['nodes'] = ', '.join(nodes)

  # links
  links = []
  for (s,d), e in G.edges.iteritems():
    s,d = min(s,d), max(s,d)
    links.append("(%s,pt%d) <-> (%s,pt%d)" % (l(s), e['src_port'], l(d), e['dst_port']))
  params['links'] = ',\n'.join(links)

  # programs
  programs = []
  for n in G.nodes:
    if n in range(k*4):
      programs.append("%s -> s%d" % (l(n), n % 4))
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
  topo = make_topo(k)
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
