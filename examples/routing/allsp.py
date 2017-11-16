import networkx as nx
from collections import defaultdict

def route(G, dst=0):
    root = dst
    print root
    all_sps = defaultdict(list)
    for s in G.nodes():
        paths = nx.all_shortest_paths(G,source=s,target=root)
        all_sps[s] = paths
    return all_sps
