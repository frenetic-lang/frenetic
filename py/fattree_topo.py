from mininet.topo import Topo, Node

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
        self.add_node(99, Node(is_switch=False))
        
        for s1 in edgeSwitches:
            if (s1 - firstSwitch) < numEdgeSwitches / 2:
                self.add_edge(s1, coreSwitches[0])
            else:
                self.add_edge(s1, coreSwitches[1])
            # connect monitor to every edge switch
            self.add_edge(99, s1)            

        self.add_edge(coreSwitches[0], coreSwitches[1])
      
        self.enable_all()
                

Topology = FattreeTopology
topos = { 'fattree_topo': ( lambda: Topology() ) }
