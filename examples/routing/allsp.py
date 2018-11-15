import networkx as nx
from collections import defaultdict

def route(G, dst=0):
    root = dst
    print root
    next_hops = defaultdict(set)
    for s in G.nodes():
	print s
        paths = nx.all_shortest_paths(G,source=s,target=root)
        try:
            for path in paths:
                if len(path) >= 2:
                    next_hops[s].add(path[1])
        except:
            pass
    return next_hops
