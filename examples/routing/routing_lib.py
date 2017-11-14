def serialize_trees(trees, outfile):
    counter = 0
    with open(outfile+'.trees', 'w') as f:
        for tree in trees:
            f.write('# TREE: ' + str(counter) + '\n')
            for (u,v) in tree:
                f.write(str(u) + ' -- ' + str(v) + '\n')
            counter += 1
