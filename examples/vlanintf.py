#!/usr/bin/env python
from mininet.link import Intf

HOST_VLAN = 1

class VLANIntf( Intf ):

    def vconfig( self, *args ):
        "Configure ourselves using vconfig"
        vlanId = HOST_VLAN
        self.cmd( 'vconfig', 'add', self.name, vlanId )
        name = '%s.%s' % (self.name, vlanId)
        return self.cmd( 'ifconfig', name, *args )

    def setIP( self, ipstr, prefixLen=None ):
        if '/' in ipstr:
            self.ip, self.prefixLen = ipstr.split( '/' )
            return self.vconfig( ipstr, 'up' )
        else:
            self.ip, self.prefixLen = ipstr, prefixLen
            return self.vconfig( '%s/%s' % ( ipstr, prefixLen ) )
