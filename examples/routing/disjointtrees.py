from sage.all import *

'''input: directed networkx graph of switches
   output: list of routing trees, where each tree is a set of edges (src, dst)'''
def route(sw_topo, root_id=0, num_trees=None):
    # Convert to SAGE graph
    G = DiGraph(sw_topo)

    e_con = G.edge_connectivity()
    if num_trees is None:
        num_trees = e_con

    for u,v,d in G.edges():
        del d['cost']
        del d['capacity']
        del d['src_port']
        del d['dst_port']

    if e_con < num_trees:
        print "Graph is not ", num_trees, "connected. Aborting."
        sys.exit()
    print e_con
    # Select root.
    vertices = G.vertices()
    idx = vertices.index(root_id)
    root = G.vertices()[idx]
    print root,type(root)

    arborescences = G.edge_disjoint_spanning_trees(num_trees, root=root,
                                                   solver='GLPK')

    trees = []
    for a in arborescences:
        tree = set()
        for u,v,d in a.edges():
            tree.add((v, u))
        trees.append(tree)
    return trees
