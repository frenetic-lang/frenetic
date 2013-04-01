from mininet.topo import Topo, Node
import random
import networkx as nx
import math

class WattsStrogatzTopology(Topo):

    def __init__(self, numSwitches=6):

        super(WattsStrogatzTopology, self).__init__()

        # add switches
        numHosts = numSwitches
        hosts = range(1, numHosts+1)
        firstSwitch = max(101, numHosts+1)
        switches = range(firstSwitch, numSwitches + firstSwitch)

        # Add switches
        for s in switches:
            self.add_node(s, Node(is_switch=True))
            
        # Add hosts
        for h in hosts:
            self.add_node(h, Node(is_switch=False))

        # Add links
        for h in hosts:
            self.add_edge(h, switches[h-1])

        rev_switches = list(switches)
        rev_switches.reverse()
        [last] = rev_switches[-1:]
        for s in rev_switches:
            self.add_edge(s, last)
            last = s


        # Add "magic" links

        self.add_edge(101, 103)
        self.add_edge(102, 105)
        
        # Add monitoring host
        # self.add_node(99, Node(is_switch=False))

        # for s in switches:
        #     self.add_edge(s, 99)
        
        self.enable_all()
                
# 4 hosts on each edge switch
# N/2 core switches

class FattreeTopology(Topo):

    def __init__(self, numEdgeSwitches=4):

        super(FattreeTopology, self).__init__()

        # add switches
        numHosts = 4*numEdgeSwitches
        numCoreSwitches = 2
        hosts = range(1, numHosts+1)
        firstSwitch = max(101, numHosts+1)
        edgeSwitches = range(firstSwitch, numEdgeSwitches + firstSwitch)
        self.edgeSwitches = edgeSwitches
        coreSwitches=range(numEdgeSwitches + firstSwitch, numEdgeSwitches + firstSwitch + numCoreSwitches)
        self.coreSwitches = coreSwitches

        # Add switches
        for s in edgeSwitches:
            self.add_node(s, Node(is_switch=True))
            
        for s in coreSwitches:
            self.add_node(s, Node(is_switch=True))
                         
        # Add hosts
        for h in hosts:
            self.add_node(h, Node(is_switch=False))

        # Add links
        for h in hosts:
            if h <= 4:
                self.add_edge(h, firstSwitch)
            elif h <= 8:
                self.add_edge(h, firstSwitch + 1)
            elif h <= 12:
                self.add_edge(h, firstSwitch + 2)
            else:
                self.add_edge(h, firstSwitch + 3)
                
        # Add monitoring host
        # self.add_node(99, Node(is_switch=False))
        
        for s1 in edgeSwitches:
            if (s1 - firstSwitch) < numEdgeSwitches / 2:
                self.add_edge(s1, coreSwitches[0])
            else:
                self.add_edge(s1, coreSwitches[1])
            # connect monitor to every edge switch
            # self.add_edge(99, s1)            

        self.add_edge(coreSwitches[0], coreSwitches[1])
      
        self.enable_all()

def waxman_graph(n, alpha=0.8, beta=0.1, L=None, domain=(0,0,1,1)):
    r"""Return a Waxman random graph.

    The Waxman random graph models place n nodes uniformly at random
    in a rectangular domain. Two nodes u,v are connected with an edge
    with probability

    .. math::
            p = \alpha*exp(d/(\beta*L)).

    This function implements both Waxman models.            

    Waxman-1:  `L` not specified
       The distance `d` is the Euclidean distance between the nodes u and v.
       `L` is the maximum distance between all nodes in the graph.

    Waxman-2: `L` specified
       The distance `d` is chosen randomly in `[0,L]`.

    Parameters
    ----------
    n : int
        Number of nodes
    alpha: float
        Model parameter
    beta: float
        Model parameter
    L : float, optional
        Maximum distance between nodes.  If not specified the actual distance
        is calculated.
    domain : tuple of numbers, optional
         Domain size (xmin, ymin, xmax, ymax)

    Returns
    -------
    G: Graph

    References
    ----------
    .. [1]  B. M. Waxman, Routing of multipoint connections. 
       IEEE J. Select. Areas Commun. 6(9),(1988) 1617-1622. 
    """
    # build graph of n nodes with random positions in the unit square
    G = nx.Graph()
    G.add_nodes_from(range(1,n+1))
    (xmin,ymin,xmax,ymax)=domain
    for n in G:
        G.node[n]['pos']=((xmin + (xmax-xmin))*random.random(),
                          (ymin + (ymax-ymin))*random.random())
    if L is None:
        # find maximum distance L between two nodes
        l = 0
        pos = [G.node[node]['pos'] for node in G]
        while pos:
            x1,y1 = pos.pop()
            for x2,y2 in pos:
                r2 = (x1-x2)**2 + (y1-y2)**2
                if r2 > l:
                    l = r2
        l=math.sqrt(l)
    else: 
        # user specified maximum distance
        l = L

    nodes=G.nodes()
    if L is None:
        # Waxman-1 model
        # try all pairs, connect randomly based on euclidean distance
        while nodes:
            u = nodes.pop()
            x1,y1 = G.node[u]['pos']
            for v in nodes:
                x2,y2 = G.node[v]['pos']
                r = math.sqrt((x1-x2)**2 + (y1-y2)**2)
                if random.random() < alpha*math.exp(-r/(beta*l)):
                    G.add_edge(u,v)
    else:
        # Waxman-2 model
        # try all pairs, connect randomly based on randomly chosen l
        while nodes:
            u = nodes.pop()
            for v in nodes:
                r = random.random()*l
                if random.random() < alpha*math.exp(-r/(beta*l)):
                    G.add_edge(u,v)
    return G

class WaxmanTopology(Topo):

    def __init__(self, num_switches=None):

        super(WaxmanTopology, self).__init__()
        
        num_hosts_per_switch = 4
        # Needed so that subsequent calls will generate the same graph
        random.seed(100)
        if not num_switches:
            num_switches = 5
        num_hosts = num_switches*num_hosts_per_switch
        # build waxman graph
        wax = waxman_graph(num_switches)

        # Add switches
        for s in wax:
            self.node(s, Node(is_switch=True))

        # Add edges
        for s1, s2 in wax.edges():
            self.add_link(s1, s2)
                         
        # Add hosts
        hostoffset = num_switches+2
        for s in wax:
            # Add host
            host_base = num_hosts_per_switch*s + hostoffset
            for host in range(0, num_hosts_per_switch):
                self.add_node(host_base + host)
                self.add_link(host_base + host, s)
                
        # # Globally connected host
        # self.add_host(9999)
        # for switch in wax:
        #     self.add_link(9999, switch)


        # f = open('/home/openflow/workspace/foo.log', 'w')
        # f.write('hosts: %d\n' % len(self.hosts()))
        # f.close()
        # assert(False)
        self.enable_all()

topos = { 
  'wattsstrogatz': ( WattsStrogatzTopology ),
  'fattree': ( FattreeTopology ),
  'waxman': ( WaxmanTopology )
}
