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

class WaxmanTopology(Topo):

    def __init__(self, num_switches=5,seed=100):

        super(WaxmanTopology, self).__init__()
        
        num_hosts_per_switch = 2
        # Needed so that subsequent calls will generate the same graph
        random.seed(seed)
        num_hosts = num_switches*num_hosts_per_switch
        # build waxman graph
        wax = nx.waxman_graph(num_switches,.9,.9)

        # Add switches
        for s in wax.nodes():
            self.add_node(s+1, Node(is_switch=True))

            
        # Add edges
        for s1, s2 in wax.edges():
            print "new edge"
            self.add_edge(s1+1, s2+1)
                         
        # Add hosts
        hostoffset = num_switches+2
        for s in wax:
            # Add host
            host_base = num_hosts_per_switch*s + hostoffset
            for host in range(0, num_hosts_per_switch):
                self.add_node(host_base + host, Node(is_switch=False))
                self.add_edge(host_base + host, s+1)
                
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
