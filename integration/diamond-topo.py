from mininet.topo import Topo

class DiamondTopo( Topo ):
    "Diamond."

    def __init__( self ):
        "Create custom topo."

        # Initialize topology
        Topo.__init__( self )

        h1 = self.addHost("h1", mac='00:00:00:00:00:01')
        h2 = self.addHost("h2", mac='00:00:00:00:00:02')
        
        s0 = self.addSwitch("s1")
        s1 = self.addSwitch("s2")
        s2 = self.addSwitch("s3")
        s3 = self.addSwitch("s4")
        
        # path top
        self.addLink(h1, s0, 1, 3)
        failme = self.addLink(s0, s1, 1, 2)
        self.addLink(s1, s3, 1, 1)
        self.addLink(s3, h2, 3, 1)
        
        #path bottom
        self.addLink(s3, s2, 2, 1)
        self.addLink(s2, s0, 2, 2)

topos = { 'diamondtopo': ( lambda: DiamondTopo() ) }
