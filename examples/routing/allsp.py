import networkx as nx
from collections import defaultdict

def route(G, dst=0):
    root = dst
    print root
    next_hops = defaultdict(set)
    for s in G.nodes():
        paths = nx.all_shortest_paths(G,source=s,target=root)
        for path in paths:
            if len(path) >= 2:
                next_hops[s].add(path[1])
    return next_hops
