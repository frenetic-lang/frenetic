from collections import defaultdict

def serialize_trees(trees, outfile):
    counter = 0
    with open(outfile+'.trees', 'w') as f:
        for tree in trees:
            f.write('# TREE: ' + str(counter) + '\n')
            for (u,v) in sorted(tree):
                f.write(str(u) + ' -- ' + str(v) + '\n')
            counter += 1

def serialize_nexthops(allnhs, outfile):
    with open(outfile+'.nexthops', 'w') as f:
        for s in sorted(allnhs.keys()):
            nhs = allnhs[s]
            f.write(str(s) + " : " + " ".join([str(v) for v in sorted(nhs)]) + '\n')

def serialize_routes(routes, outfile):
    if type(routes) == list:
        serialize_trees(routes, outfile)
    elif type(routes) == defaultdict:
        serialize_nexthops(routes, outfile)

