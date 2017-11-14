import networkx as nx
import spf
import disjointtrees

def get_topo():
    g = nx.DiGraph()
    g.add_nodes_from([0,1,2,3,4,5])
    g.add_edges_from([(0,1), (0,2), (0,3),
                      (1,0), (2,0), (3,0),
                      (1,2), (1,4), (1,6),
                      (2,1), (4,1), (6,1),
                      (3,4), (3,6),
                      (4,3), (6,3),
                      (4,5),
                      (5,4),
                      (5,6),
                      (6,5)
                      ])
    print 'hi', g.nodes().items()
    return g

def run_spf(g):
    trees = spf.route(g, 0)
    print trees

def run_disjointtrees(g):
    trees = disjointtrees.route(g, 0)
    print trees

if __name__=='__main__':
    topo = get_topo()
    run_spf(topo)
    run_disjointtrees(topo)
