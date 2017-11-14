import networkx as nx

def route(G, dst=0):
    root = dst
    print root
    paths = nx.single_source_dijkstra_path(G,root)
    tree = set()
    for v,p in paths.iteritems():
        if len(p) < 2:
            continue
        else:
            edges = zip(p[1:], p[:-1])
            tree |= set(edges)
    return [tree]
