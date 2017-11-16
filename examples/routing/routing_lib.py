from collections import defaultdict

def serialize_trees(trees, outfile):
    counter = 0
    with open(outfile+'.trees', 'w') as f:
        for tree in trees:
            f.write('# TREE: ' + str(counter) + '\n')
            for (u,v) in tree:
                f.write(str(u) + ' -- ' + str(v) + '\n')
            counter += 1

def serialize_paths(allpaths, outfile):
    with open(outfile+'.allsp', 'w') as f:
        for s,paths in allpaths.iteritems():
            f.write('# Source: ' + str(s) + '\n')
            for path in paths:
                f.write(" -- ".join([str(v) for v in path]) + '\n')

def serialize_routes(routes, outfile):
    if type(routes) == list:
        serialize_trees(routes, outfile)
    elif type(routes) == defaultdict:
        serialize_paths(routes, outfile)

