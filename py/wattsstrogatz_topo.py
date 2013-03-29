from mininet.topo import Topo, Node

class WattsStrogatzTopology(Topo):

    def __init__(self):

        super(WattsStrogatzTopology, self).__init__()

        # add switches
        numSwitches = 6
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
        self.add_node(99, Node(is_switch=False))

        for s in switches:
            self.add_edge(s, 99)
        
        self.enable_all()
                

Topology = WattsStrogatzTopology
topos = { 'wattsstrogatz_topo': ( lambda: Topology() ) }
