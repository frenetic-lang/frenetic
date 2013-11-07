#!/usr/bin/python

'''this file generates an AB fat tree in dot notation with few assumptions:
1) fan out is multiple of 2
2) all switches have same incoming and outgoing edges, except the root nodes
3) all switches have same fan out '''

'''
{rank = same; "s17"; "s18"; "s19"; "s20"; }
{rank = same; "s9"; "s10"; "s11"; "s12"; "s13"; "s14"; "s15"; "s16"; }
{rank = same; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; }
{rank = same; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "h7"; "h8"; "h9"; "h10"; "h11"; "h12"; "h13"; "h14"; "h15"; "h16"; }
'''

import sys
import argparse
import pprint
import string
import networkx as nx

def generate(fanout,depth):
    L = depth - 1
    p = fanout / 2
    switches = ['s'+ str(i) for i in range(1, ((2*L + 1) * (p ** L)+1))]
    hosts = ['h' + str(i) for i in range(1, 2 * (p ** (L+1)) + 1)]
    nodes = switches + hosts
    graph = nx.DiGraph()
    for node in switches:
        graph.add_node(node, type='switch', id=int(node[1:]))
    for node in hosts:
        graph.add_node(node, type='host', mac="00:00:00:00:00:%02x" % (int(node[1:])), ip="10.0.0.%d" % (int(node[1:])))


    graph.switches = switches
    graph.hosts = hosts
    graph.p = p
    graph.L = L

    for idx in range(2 * (p ** (depth-1))):
        node = nodes[idx]
        c = 1
        for j in range(idx*p, idx*p + p):
            hostnode = hosts[j]
            graph.add_edge(hostnode, node,
                attr_dict={'sport':1,'dport':c,'capacity':'1Gbps','cost':'1'})
            graph.add_edge(node, hostnode,
                attr_dict={'sport':c,'dport':1,'capacity':'1Gbps','cost':'1'})
            #print "dport: %d" % (c)
            c += 1

    for i in range(L):
        groups = 2*(p ** (L - i))
        for g in range(groups):
            '''type A = 0; type B = 1'''
            sttype = g % 2
            for j in range(p ** i):
                idx = i * 2 * (p ** L) + g * (p ** i) + j
                #print "i, g, j, idx, sstype: %d %d %d %d %d" % (i, g, j, idx, sttype)
                node = nodes[idx]
                if i < L - 1:
                    parentsg = g / p
                else:
                    parentsg = 0
                parentbase = (i + 1) * 2 * (p ** L) + parentsg * (p ** (i + 1))
                #print "parentbase: %d" % (parentbase)
                if sttype == 0:
                    parentbase = parentbase + j * p
                    parentsidxs = range(parentbase, parentbase + p)
                else:
                    parentsidxs = [parentbase + j + x * (p ** i) for x in range(0, p)]
                #print parentsidxs
                c = 1
                assert len(parentsidxs) == p
                for pidx in parentsidxs:
                    parentnode = nodes[pidx]
                    sport = p + c
                    c += 1
                    if i < L - 1:
                        dport = (g % p) + 1
                    else:
                        dport = g + 1
                    assert sport <= 2*p
                    assert dport <= 2*p
                    #print "dport: %d" % (dport)
                    graph.add_edge(node, parentnode,
                        attr_dict={'sport':sport,'dport':dport,'capacity':'1Gbps','cost':'1'})
                    graph.add_edge(parentnode, node,
                        attr_dict={'sport':dport,'dport':sport,'capacity':'1Gbps','cost':'1'})
    return graph

def rec_routing_downwards(graph, node, host, level):
    upwards = [x for x in graph.neighbors_iter(node) if (x not in graph.hosts and graph.node[x]['level'] == level)]
    for upnode in upwards:
        e = graph.get_edge_data(node, upnode)
        graph.node[upnode]['routes'][host] = e['dport']
        if level+1 <= graph.L:
            rec_routing_downwards(graph, upnode, host, level+1)

def routing_upwards(graph, node):
    # by convention, port > p is facing upwards except for level L
    for port in range(1, graph.p+1):
        graph.node[node]['routes'][port] = graph.p + port

def to_netkat_set_of_tables(graph):
    policy = []
    for node in graph.switches:
        table = []
        flt = "filter switch = %d" % (graph.node[node]['id'])
        #pprint.pprint(graph.node[node]['routes'])
        for k, v in graph.node[node]['routes'].iteritems():
            if k in graph.hosts:
                s = string.join((flt, "filter ethDst = %s" % (graph.node[k]['mac']), "port := %d" % (v)), "; ")
            else:
                s = string.join((flt, "filter port = %d" % (k), "port := %d" % (v)), "; ")
            table.append(s)
        #pprint.pprint(table)
        policy.extend(table)
    topo = []
    for src, dst, ed in graph.edges_iter(data=True):
        if src in graph.hosts or dst in graph.hosts:
            continue
        topoterm = "%s@%d => %s@%d" % (graph.node[src]['id'], ed['sport'], graph.node[dst]['id'], ed['dport'])
        topo.append(topoterm)
        #topoterm = "%s@%d => %s@%d" % (graph.node[dst]['id'], ed['dport'], graph.node[src]['id'], ed['sport'])
        #topo.append(topoterm)


    return "(\n%s\n);\n(\n%s\n)" % (string.join(policy, " |\n"), string.join(topo, " |\n"))

def find_next_node(graph, node, outport):
    #print "find_next_node", node, outport
    for src, dst, ed in graph.edges_iter([node], data=True):
        assert src == node
        if graph.node[dst]['type'] == 'host':
            continue
        if ed['sport'] == outport:
            return (dst, ed['dport'])

def rec_set_of_paths_next_hop(graph, node, inport, dst, srchost, dsthost, switches):
    path = []
    flt = "filter switch = %d" % (graph.node[node]['id'])
    switches.add(node)
    if node == dst:
        assert dsthost in graph.node[dst]['routes']

    if dsthost in graph.node[node]['routes']:
        v = graph.node[node]['routes'][dsthost]
        path = [string.join((flt, "port := %d" % (v)), "; ")]
    else:
        assert inport in graph.node[node]['routes']
        #print "inport", inport
        v = graph.node[node]['routes'][inport]
        path = [string.join((flt, "port := %d" % (v)), "; ")]
    if node != dst:
        nextnode, nextinport = find_next_node(graph, node, v)
        #print "next", nextnode, nextinport
        path.extend(rec_set_of_paths_next_hop(graph, nextnode, nextinport, dst, srchost, dsthost, switches))
    return path

def to_netkat_set_of_paths_for_hosts(graph, hosts):
    policy = []
    switches = set()
    for srchost in hosts:
        src = graph.neighbors(srchost)[0]
        nextnode, inport = find_next_node(graph, srchost, 1)
        assert src == nextnode
        for dsthost in hosts:
            if srchost == dsthost:
                continue
            dst = graph.neighbors(dsthost)[0]
            flt = "filter ethSrc = %s; filter ethDst = %s" % (graph.node[srchost]['mac'], graph.node[dsthost]['mac'])
            #print "path", srchost, dsthost
            path = rec_set_of_paths_next_hop(graph, src, inport, dst, srchost, dsthost, switches)
            #print string.join(path, " | ")
            policy.append("(%s; ( %s ))" % (flt, string.join(path, " | ")))
    topo = []
    for src, dst, ed in graph.edges_iter(data=True):
        if src in graph.hosts or dst in graph.hosts:
            continue
        if src not in switches or dst not in switches:
            continue
        topoterm = "%s@%d => %s@%d" % (graph.node[src]['id'], ed['sport'], graph.node[dst]['id'], ed['dport'])
        topo.append(topoterm)
        #topoterm = "%s@%d => %s@%d" % (graph.node[dst]['id'], ed['dport'], graph.node[src]['id'], ed['sport'])
        #topo.append(topoterm)


    return "(\n%s\n);\n(\n%s\n)" % (string.join(policy, " |\n"), string.join(topo, " |\n"))

def to_netkat_set_of_paths(graph):
    return to_netkat_set_of_paths_for_hosts(graph, graph.hosts)

def to_netkat_test_set_of_paths(graph):
    return to_netkat_set_of_paths_for_hosts(graph, graph.hosts[0:2])

def to_netkat_test_set_of_paths2(graph):
    return to_netkat_set_of_paths_for_hosts(graph, graph.hosts[0:3])

def to_netkat(graph, kattype, katfile):
    for node in graph.switches:
        graph.node[node]['routes'] = {}
        l = (graph.node[node]['id'] - 1) / (2*graph.p**graph.L)
        graph.node[node]['level'] = l
    for node in graph.hosts:
        rec_routing_downwards(graph, node, node, 0)
    n = len(graph.switches) - graph.p**graph.L
    noncore_switches = graph.switches[0:n]
    for node in noncore_switches:
        routing_upwards(graph, node)
    #print nx.to_agraph(graph)
    if kattype == 'tables':
        policy = to_netkat_set_of_tables(graph)
    elif kattype == 'paths':
        policy = to_netkat_set_of_paths(graph)
    elif kattype == 'testpaths':
        policy = to_netkat_test_set_of_paths(graph)
    elif kattype == 'testpaths2':
        policy = to_netkat_test_set_of_paths2(graph)
    if katfile:
        with open(katfile, 'w') as f:
            f.write(policy)
    else:
        print policy

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("fanout", type=int,
                        help="number of children each node should have")
    parser.add_argument("depth", type=int,
                        help="depth of the fattree")
    parser.add_argument("-o", "--out", dest='output', action='store',
                        default=None,
                        help='file to write to')
    parser.add_argument("-k", "--kat", dest='katfile', action='store',
                        default=None,
                        help='file to write to')
    parser.add_argument("-t", "--type",
                        help='KAT policy type',
                        dest='kattype',
                        action='store',
                        choices=['tables', 'paths', 'testpaths', 'testpaths2'],
                        default='tables',
                        type=str)

    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    graph = generate(args.fanout, args.depth)

    if args.output:
        nx.write_dot(graph,args.output)
    else:
        print nx.to_agraph(graph)
    to_netkat(graph, args.kattype, args.katfile)
